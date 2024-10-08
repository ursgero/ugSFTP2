
# ugSFTP2 SFTP Client

Der `ugSFTP2` ist ein leistungsstarker und sicherer SFTP-Client, entwickelt, um Dateien zwischen einem lokalen und einem entfernten Verzeichnis unter Verwendung des Secure File Transfer Protocol (SFTP) zu synchronisieren. Dieses Projekt ist in Free Pascal geschrieben und nutzt eine C++-Bibliothek für kritische SFTP-Operationen, was eine effiziente und sichere Datenübertragung gewährleistet.

## Features

- **Sichere Datenübertragung:** Nutzt SSH für alle Netzwerkoperationen.
- **Unterstützung für mehrere Plattformen:** Entwickelt für Cross-Plattform-Kompatibilität.
- **Mehrsprachige Unterstützung:** Hilfe-Texte sind in mehreren Sprachen verfügbar (Englisch, Deutsch, Französisch, Italienisch, Spanisch, Rumantsch und Schweizerdeutsch).
- **Flexible Konfiguration:** Konfigurierbar über die Kommandozeile oder über eine externe Konfigurationsdatei.

## Voraussetzungen

- Free Pascal Compiler
- C++ Compiler (z.B. GCC oder Clang)
- libssh und libssh/sftp-Bibliotheken für die C++-Implementierung

## Installation

1. **Klonen Sie das Repository:**

   git clone https://github.com/IhrUsername/ugSFTP2.git
   cd ugSFTP2


2. **Kompilieren der C++-Bibliothek:**

   cd sftp-lib-cpp
   make


3. **Kompilieren des Pascal-Clients:**

   cd ../
   fpc -Fu./ugSFTPunit -Fu./HelpTexts -Fl./sftp-lib-cpp ugSFTP2.pas


## Konfiguration

Die Konfiguration kann entweder durch eine externe Datei `config.txt` oder durch direkte Kommandozeilenargumente erfolgen.

### Konfigurationsdatei (config.txt)

locdir=/path/to/local/dir
remdir=/path/to/remote/dir
server=example.com
user=username
password=secret


## Benutzung

./ugSFTP2 --server example.com --user <user> --password <password> --locdir /local/dir --remdir /remote/dir
./ugSFTP2 -c /path/to/config.txt

Für mehrsprachige Hilfe:

./ugSFTP2 --help    # Standard in Englisch
./ugSFTP2 --help-de # Für deutsche Hilfe


## Testen

Um das Programm zu testen:


sudo bash -c 'LD_LIBRARY_PATH=./sftp-lib-cpp:$LD_LIBRARY_PATH gdb ./ugSFTP2'


Dann mit `run` im Debugger starten.

## Beitrag

Beiträge zu `ugSFTP2` sind herzlich willkommen. Sie können das Projekt forken und Ihre eigenen Features hinzufügen oder Verbesserungsvorschläge und Fehler als Issues einreichen.

## Lizenz

Dieses Projekt steht unter der MIT Lizenz. Weitere Informationen finden Sie in der Datei `LICENSE`.

## Kontakt

Für weitere Informationen kontaktieren Sie bitte [IhrName](mailto:ihre.email@example.com).
