#include "UgSFTPClient.h"
#include <vector>
#include <cstring> // Für strdup und free
#include <iostream>  // Include für std::cout

extern "C" {

    struct FileInfoC {
        char* name; // Verwenden Sie char* anstelle von const char* für den Zugriff in Pascal
        uint64_t size;
        int permissions;
        long mtime;
    };

    UgSFTPClient* UgSFTPClientCreate(const char* host, int port, const char* username, const char* password) {
        return new UgSFTPClient(host, port, username, password);
    }

    void UgSFTPClientDestroy(UgSFTPClient* client) {
        delete client;
    }

    bool UgSFTPClientConnect(UgSFTPClient* client) {
        return client->connect();
    }

    char* UgSFTPClientListFiles(UgSFTPClient* client, const char* directory) {
        std::vector<FileInfo> fileList;
        if (!client->listFiles(directory, fileList)) {
            return nullptr;  // Return null if unable to list files
        }

        std::string result;
        for (const auto& file : fileList) {
            result += "\"Name\"=\"" + file.name +
                      "\";\"Groesse\"=\"" + std::to_string(file.size) +
                      "\";\"mdate\"=\"" + std::to_string(file.mtime) + "\",";
        }

        // Remove the last comma
        if (!result.empty()) result.pop_back();

        char* c_result = new char[result.length() + 1];
        std::strcpy(c_result, result.c_str());
        return c_result;  // This needs to be freed in Pascal using the provided free function
    }

    void UgSFTPClientFreeString(char* str) {
        delete[] str;
    }

    bool UgSFTPClientUploadFile(UgSFTPClient* client, const char* localPath, const char* remotePath) {
        return client->uploadFile(localPath, remotePath);
    }

    bool UgSFTPClientDownloadFile(UgSFTPClient* client, const char* remotePath, const char* localPath) {
        return client->downloadFile(remotePath, localPath);
    }
}
