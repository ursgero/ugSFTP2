#include "UgSFTPClient.h"

UgSFTPClient::UgSFTPClient(const std::string &host, int port, const std::string &username, const std::string &password)
    : host_(host), port_(port), username_(username), password_(password), session_(nullptr), sftp_(nullptr) {}

UgSFTPClient::~UgSFTPClient() {
    disconnect();
}

bool UgSFTPClient::connect() {
    if (!initSession())
        return false;
    return initSFTP();
}

void UgSFTPClient::disconnect() {
    if (sftp_) {
        sftp_free(sftp_);
        sftp_ = nullptr;
    }
    if (session_) {
        ssh_disconnect(session_);
        ssh_free(session_);
        session_ = nullptr;
    }
}

bool UgSFTPClient::initSession() {
    session_ = ssh_new();
    if (session_ == nullptr) {
        std::cerr << "Error creating SSH session." << std::endl;
        return false;
    }
    ssh_options_set(session_, SSH_OPTIONS_HOST, host_.c_str());
    ssh_options_set(session_, SSH_OPTIONS_PORT, &port_);
    ssh_options_set(session_, SSH_OPTIONS_USER, username_.c_str());

    int rc = ssh_connect(session_);
    if (rc != SSH_OK) {
        std::cerr << "Error connecting: " << ssh_get_error(session_) << std::endl;
        ssh_free(session_);
        session_ = nullptr;
        return false;
    }

    rc = ssh_userauth_password(session_, nullptr, password_.c_str());
    if (rc != SSH_AUTH_SUCCESS) {
        std::cerr << "Authentication failed: " << ssh_get_error(session_) << std::endl;
        ssh_disconnect(session_);
        ssh_free(session_);
        session_ = nullptr;
        return false;
    }

    return true;
}

bool UgSFTPClient::initSFTP() {
    sftp_ = sftp_new(session_);
    if (sftp_ == nullptr) {
        std::cerr << "Error creating SFTP session: " << ssh_get_error(session_) << std::endl;
        return false;
    }

    int rc = sftp_init(sftp_);
    if (rc != SSH_OK) {
        std::cerr << "Error initializing SFTP session: " << sftp_get_error(sftp_) << std::endl;
        sftp_free(sftp_);
        sftp_ = nullptr;
        return false;
    }

    return true;
}

bool UgSFTPClient::listFiles(const std::string &directory, std::vector<FileInfo> &fileList) {
    if (sftp_ == nullptr) {
        std::cerr << "SFTP session is not initialized." << std::endl;
        return false;
    }

    sftp_dir dir;
    sftp_attributes attributes;
    dir = sftp_opendir(sftp_, directory.c_str());
    if (dir == nullptr) {
        std::cerr << "Unable to open directory: " << directory << " Error: " << sftp_get_error(sftp_) << std::endl;
        return false;
    }

    while ((attributes = sftp_readdir(sftp_, dir)) != nullptr) {
        FileInfo info;
        info.name = attributes->name;
        info.size = attributes->size;
        info.permissions = attributes->permissions;
        info.mtime = attributes->mtime;  // Stellen Sie sicher, dass mtime korrekt zugewiesen wird

        fileList.push_back(info);
        sftp_attributes_free(attributes);
    }

    if (!sftp_dir_eof(dir)) {
        std::cerr << "Can not list all files in directory: " << directory << " Error: " << sftp_get_error(sftp_) << std::endl;
        sftp_closedir(dir);
        return false;
    }

    int rc = sftp_closedir(dir);
    if (rc != SSH_OK) {
        std::cerr << "Unable to close directory: " << directory << " Error: " << sftp_get_error(sftp_) << std::endl;
        return false;
    }

    return true;
}

bool UgSFTPClient::uploadFile(const std::string &localPath, const std::string &remotePath) {
    if (sftp_ == nullptr) {
        std::cerr << "SFTP session is not initialized." << std::endl;
        return false;
    }

    // Öffnen der lokalen Datei
    int localFile = open(localPath.c_str(), O_RDONLY);
    if (localFile < 0) {
        std::cerr << "Unable to open local file: " << localPath << std::endl;
        return false;
    }

    // Erstellen einer Remote-Datei
    sftp_file file = sftp_open(sftp_, remotePath.c_str(),
                               O_WRONLY | O_CREAT | O_TRUNC, S_IRWXU);
    if (file == nullptr) {
        std::cerr << "Unable to open remote file: " << remotePath << " Error: " << sftp_get_error(sftp_) << std::endl;
        close(localFile);
        return false;
    }

    // Lesen von der lokalen Datei und Schreiben in die Remote-Datei
    char buffer[4096];
    ssize_t bytesRead, bytesWritten;
    while ((bytesRead = read(localFile, buffer, sizeof(buffer))) > 0) {
        bytesWritten = sftp_write(file, buffer, bytesRead);
        if (bytesWritten != bytesRead) {
            std::cerr << "Error writing to remote file: " << remotePath << std::endl;
            sftp_close(file);
            close(localFile);
            return false;
        }
    }

    if (bytesRead < 0) {
        std::cerr << "Error reading local file: " << localPath << std::endl;
        sftp_close(file);
        close(localFile);
        return false;
    }

    // Schließen der Dateien
    if (sftp_close(file) != SSH_OK) {
        std::cerr << "Unable to close the remote file: " << remotePath << std::endl;
        close(localFile);
        return false;
    }

    close(localFile);
    return true;
}

bool UgSFTPClient::downloadFile(const std::string &remotePath, const std::string &localPath) {
    if (sftp_ == nullptr) {
        std::cerr << "SFTP session is not initialized." << std::endl;
        return false;
    }

    // Öffnen der Remote-Datei zum Lesen
    sftp_file file = sftp_open(sftp_, remotePath.c_str(), O_RDONLY, 0);
    if (file == nullptr) {
        std::cerr << "Unable to open remote file: " << remotePath << " Error: " << sftp_get_error(sftp_) << std::endl;
        return false;
    }

    // Erstellen einer lokalen Datei zum Schreiben
    int localFile = open(localPath.c_str(), O_WRONLY | O_CREAT | O_TRUNC, S_IRWXU);
    if (localFile < 0) {
        std::cerr << "Unable to create local file: " << localPath << std::endl;
        sftp_close(file);
        return false;
    }

    // Lesen von der Remote-Datei und Schreiben in die lokale Datei
    char buffer[4096];
    ssize_t bytesRead, bytesWritten;
    while ((bytesRead = sftp_read(file, buffer, sizeof(buffer))) > 0) {
        bytesWritten = write(localFile, buffer, bytesRead);
        if (bytesWritten != bytesRead) {
            std::cerr << "Error writing to local file: " << localPath << std::endl;
            sftp_close(file);
            close(localFile);
            return false;
        }
    }

    if (bytesRead < 0) {
        std::cerr << "Error reading remote file: " << remotePath << std::endl;
        sftp_close(file);
        close(localFile);
        return false;
    }

    // Schließen der Dateien
    if (sftp_close(file) != SSH_OK) {
        std::cerr << "Unable to close the remote file: " << remotePath << std::endl;
        close(localFile);
        return false;
    }

    close(localFile);
    return true;
}
