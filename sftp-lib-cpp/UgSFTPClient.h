#ifndef UGSFTPCLIENT_H
#define UGSFTPCLIENT_H

#include <libssh/libssh.h>
#include <libssh/sftp.h>
#include <string>
#include <vector>
#include <sys/stat.h>
#include <fcntl.h>
#include <cstdlib>
#include <iostream>

// Struktur für Dateiinformationen
struct FileInfo {
    std::string name;
    uint64_t size;
    int permissions;
    long mtime;
};

// Klasse für SFTP-Operationen
class UgSFTPClient {
public:
    UgSFTPClient(const std::string &host, int port, const std::string &username, const std::string &password);
    ~UgSFTPClient();

    bool connect();
    bool listFiles(const std::string &directory, std::vector<FileInfo> &fileList);
    bool uploadFile(const std::string &localPath, const std::string &remotePath);
    bool downloadFile(const std::string &remotePath, const std::string &localPath);
    void disconnect();

private:
    std::string host_;
    int port_;
    std::string username_;
    std::string password_;
    ssh_session session_;
    sftp_session sftp_;

    bool initSession();
    bool initSFTP();
};

#endif // UGSFTPCLIENT_H
