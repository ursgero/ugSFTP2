unit HelpTexts;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure ShowHelp(Language: string);

implementation

procedure ShowHelp(Language: string);
begin
  case LowerCase(Language) of
    'en', '': // English or default help
      begin
        WriteLn('Usage: ugSFTP2 [options]');
        WriteLn('Options:');
        WriteLn('  -s, --server <address>       Specify the SFTP server address. Default: localhost');
        WriteLn('  -u, --user <username>        Specify the username for the server connection. Default: user');
        WriteLn('  -p, --password <password>    Specify the password for the server connection.');
        WriteLn('  -l, --locdir <local dir>     Specify the local directory from which files will be synchronized.');
        WriteLn('  -r, --remdir <remote dir>    Specify the remote directory to which files will be synchronized.');
        WriteLn('  -c, --configfile <filepath>  Specify an alternative path to the configuration file.');
        WriteLn('  -h, --help               Show this help message and exit.');
        WriteLn('  -h-??, --help-??         Show this help message in language ( ?? = de, fr, it, pt, es, ch, rt) and exit.');
        WriteLn;
        WriteLn('Description:');
        WriteLn('  ugSFTP2 synchronizes files between a local and a remote directory');
        WriteLn('  using SFTP. It is designed to handle file transfers securely and');
        WriteLn('  efficiently. You can specify all required information via command');
        WriteLn('  line arguments or a configuration file, which provides flexibility');
        WriteLn('  and ease of use in automated scripts.');
        WriteLn;
        WriteLn('Examples:');
        WriteLn('  ugSFTP2 --server example.com --user myuser --password mypass --locdir /local/dir --remdir /remote/dir');
        WriteLn('  ugSFTP2 -c /path/to/config.txt');
        WriteLn('  For more information on configuration file format, refer to the user manual.');
        WriteLn;
        WriteLn('The configuration file should be a plain text file with key=value pairs on separate lines.');
        WriteLn('Example configuration file:');
        WriteLn('  locdir=/path/to/local/dir');
        WriteLn('  remdir=/path/to/remote/dir');
        WriteLn('  rmhost=example.com');
        WriteLn('  person=username');
        WriteLn('  kaewor=secret');
        WriteLn('Each line specifies one setting. Lines starting with # are treated as comments and ignored.');
      end;
    'de': // German help
      begin
          WriteLn('Verwendung: ugSFTP2 [Optionen]');
          WriteLn('Optionen:');
          WriteLn('  -s, --server <Adresse>       Gibt die Adresse des SFTP-Servers an. Standard: localhost');
          WriteLn('  -u, --user <Benutzername>    Gibt den Benutzernamen für die Serververbindung an. Standard: user');
          WriteLn('  -p, --password <Passwort>    Gibt das Passwort für die Serververbindung an.');
          WriteLn('  -l, --locdir <lokales Verz.> Gibt das lokale Verzeichnis an, von dem die Dateien synchronisiert werden.');
          WriteLn('  -r, --remdir <entferntes Verz.> Gibt das entfernte Verzeichnis an, in das die Dateien synchronisiert werden.');
          WriteLn('  -c, --configfile <Dateipfad> Gibt einen alternativen Pfad zur Konfigurationsdatei an.');
          WriteLn('  -h, --help                   Zeigt diese Hilfe an und beendet das Programm.');
          WriteLn('  -h-??, --help-??             Zeigt diese Hilfe in der Sprache an ( ?? = de, fr, it, pt, es, ch, rt) und beendet das Programm.');
          WriteLn;
          WriteLn('Beschreibung:');
          WriteLn('  ugSFTP2 synchronisiert Dateien zwischen einem lokalen und einem entfernten Verzeichnis');
          WriteLn('  unter Verwendung von SFTP. Es ist darauf ausgelegt, Dateiübertragungen sicher und');
          WriteLn('  effizient zu handhaben. Sie können alle erforderlichen Informationen über Befehlszeilenargumente');
          WriteLn('  oder eine Konfigurationsdatei angeben, was Flexibilität und Benutzerfreundlichkeit in automatisierten Skripten bietet.');
          WriteLn;
          WriteLn('Beispiele:');
          WriteLn('  ugSFTP2 --server example.com --user myuser --password mypass --locdir /local/dir --remdir /remote/dir');
          WriteLn('  ugSFTP2 -c /pfad/zur/config.txt');
          WriteLn('  Für weitere Informationen zum Format der Konfigurationsdatei, siehe Benutzerhandbuch.');
          WriteLn;
          WriteLn('Die Konfigurationsdatei sollte eine einfache Textdatei mit Schlüssel=Wert-Paaren auf getrennten Zeilen sein.');
          WriteLn('Beispiel für eine Konfigurationsdatei:');
          WriteLn('  locdir=/pfad/zum/lokalen/verzeichnis');
          WriteLn('  remdir=/pfad/zum/entfernten/verzeichnis');
          WriteLn('  rmhost=example.com');
          WriteLn('  person=benutzername');
          WriteLn('  kaewor=geheim');
          WriteLn('Jede Zeile gibt eine Einstellung an. Zeilen, die mit # beginnen, werden als Kommentare behandelt und ignoriert.');
      end;
    'fr': // French help
      begin
          WriteLn('Utilisation : ugSFTP2 [options]');
          WriteLn('Options :');
          WriteLn('  -s, --server <adresse>       Spécifiez l''adresse du serveur SFTP. Par défaut : localhost');
          WriteLn('  -u, --user <utilisateur>     Spécifiez le nom d''utilisateur pour la connexion au serveur. Par défaut : user');
          WriteLn('  -p, --password <mot de passe> Spécifiez le mot de passe pour la connexion au serveur.');
          WriteLn('  -l, --locdir <répertoire local> Spécifiez le répertoire local depuis lequel les fichiers seront synchronisés.');
          WriteLn('  -r, --remdir <répertoire distant> Spécifiez le répertoire distant vers lequel les fichiers seront synchronisés.');
          WriteLn('  -c, --configfile <chemin du fichier de configuration> Spécifiez un chemin alternatif pour le fichier de configuration.');
          WriteLn('  -h, --help                   Affichez ce message d''aide et quittez.');
          WriteLn('  -h-??, --help-??             Affichez ce message d''aide en langue ( ?? = de, fr, it, pt, es, ch, rt) et quittez.');
          WriteLn;
          WriteLn('Description :');
          WriteLn('  ugSFTP2 synchronise les fichiers entre un répertoire local et un répertoire distant');
          WriteLn('  en utilisant SFTP. Il est conçu pour gérer les transferts de fichiers de manière sécurisée et');
          WriteLn('  efficace. Vous pouvez spécifier toutes les informations nécessaires via des arguments de ligne de commande');
          WriteLn('  ou un fichier de configuration, ce qui offre flexibilité et facilité d''utilisation dans les scripts automatisés.');
          WriteLn;
          WriteLn('Exemples :');
          WriteLn('  ugSFTP2 --server example.com --user myuser --password mypass --locdir /local/dir --remdir /remote/dir');
          WriteLn('  ugSFTP2 -c /chemin/vers/config.txt');
          WriteLn('  Pour plus d''informations sur le format du fichier de configuration, consultez le manuel utilisateur.');
          WriteLn;
          WriteLn('Le fichier de configuration doit être un fichier texte simple avec des paires clé=valeur sur des lignes séparées.');
          WriteLn('Exemple de fichier de configuration :');
          WriteLn('  locdir=/chemin/vers/répertoire/local');
          WriteLn('  remdir=/chemin/vers/répertoire/distant');
          WriteLn('  rmhost=example.com');
          WriteLn('  person=utilisateur');
          WriteLn('  kaewor=secret');
          WriteLn('Chaque ligne spécifie un paramètre. Les lignes commençant par # sont traitées comme des commentaires et ignorées.');
      end;
    'it': // Italian help
    begin
      WriteLn('Utilizzo: ugSFTP2 [opzioni]');
      WriteLn('Opzioni:');
      WriteLn('  -s, --server <indirizzo>       Specifica l''indirizzo del server SFTP. Predefinito: localhost');
      WriteLn('  -u, --user <nome utente>       Specifica il nome utente per la connessione al server. Predefinito: user');
      WriteLn('  -p, --password <password>      Specifica la password per la connessione al server.');
      WriteLn('  -l, --locdir <dir locale>      Specifica la directory locale da cui sincronizzare i file.');
      WriteLn('  -r, --remdir <dir remota>      Specifica la directory remota verso cui sincronizzare i file.');
      WriteLn('  -c, --configfile <percorso file> Specifica un percorso alternativo per il file di configurazione.');
      WriteLn('  -h, --help                     Mostra questo messaggio di aiuto ed esce.');
      WriteLn('  -h-??, --help-??               Mostra questo messaggio di aiuto in lingua ( ?? = de, fr, it, pt, es, ch, rt) ed esce.');
      WriteLn;
      WriteLn('Descrizione:');
      WriteLn('  ugSFTP2 sincronizza file tra una directory locale e una remota');
      WriteLn('  utilizzando SFTP. È progettato per gestire trasferimenti di file in modo sicuro ed');
      WriteLn('  efficiente. Puoi specificare tutte le informazioni necessarie tramite argomenti');
      WriteLn('  della linea di comando o un file di configurazione, il che offre flessibilità');
      WriteLn('  e facilità d''uso in script automatizzati.');
      WriteLn;
      WriteLn('Esempi:');
      WriteLn('  ugSFTP2 --server example.com --user myuser --password mypass --locdir /dir/locale --remdir /dir/remota');
      WriteLn('  ugSFTP2 -c /percorso/al/config.txt');
      WriteLn('  Per maggiori informazioni sul formato del file di configurazione, consultare il manuale utente.');
      WriteLn;
      WriteLn('Il file di configurazione dovrebbe essere un file di testo semplice con coppie chiave=valore su righe separate.');
      WriteLn('Esempio di file di configurazione:');
      WriteLn('  locdir=/percorso/alla/dir/locale');
      WriteLn('  remdir=/percorso/alla/dir/remota');
      WriteLn('  rmhost=example.com');
      WriteLn('  person=nomeutente');
      WriteLn('  kaewor=segreto');
      WriteLn('Ogni riga specifica un''impostazione. Le righe che iniziano con # vengono trattate come commenti e ignorate.');
    end;

    'es': // Spanish help
    begin
      WriteLn('Uso: ugSFTP2 [opciones]');
      WriteLn('Opciones:');
      WriteLn('  -s, --server <dirección>       Especifica la dirección del servidor SFTP. Predeterminado: localhost');
      WriteLn('  -u, --user <usuario>           Especifica el nombre de usuario para la conexión al servidor. Predeterminado: usuario');
      WriteLn('  -p, --password <contraseña>    Especifica la contraseña para la conexión al servidor.');
      WriteLn('  -l, --locdir <dir local>       Especifica el directorio local desde donde se sincronizarán los archivos.');
      WriteLn('  -r, --remdir <dir remota>      Especifica el directorio remoto hacia donde se sincronizarán los archivos.');
      WriteLn('  -c, --configfile <ruta del archivo> Especifica una ruta alternativa para el archivo de configuración.');
      WriteLn('  -h, --help                     Muestra este mensaje de ayuda y sale.');
      WriteLn('  -h-??, --help-??               Muestra este mensaje de ayuda en el idioma especificado ( ?? = de, fr, it, pt, es, ch, rt) y sale.');
      WriteLn;
      WriteLn('Descripción:');
      WriteLn('  ugSFTP2 sincroniza archivos entre un directorio local y uno remoto');
      WriteLn('  utilizando SFTP. Está diseñado para manejar transferencias de archivos de manera segura y');
      WriteLn('  eficiente. Puedes especificar toda la información necesaria a través de argumentos de línea de comando');
      WriteLn('  o un archivo de configuración, lo cual proporciona flexibilidad');
      WriteLn('  y facilidad de uso en scripts automatizados.');
      WriteLn;
      WriteLn('Ejemplos:');
      WriteLn('  ugSFTP2 --server example.com --user miusuario --password mipass --locdir /dir/local --remdir /dir/remota');
      WriteLn('  ugSFTP2 -c /ruta/al/archivo/config.txt');
      WriteLn('  Para más información sobre el formato del archivo de configuración, consulta el manual del usuario.');
      WriteLn;
      WriteLn('El archivo de configuración debe ser un archivo de texto plano con pares clave=valor en líneas separadas.');
      WriteLn('Ejemplo de archivo de configuración:');
      WriteLn('  locdir=/ruta/a/dir/local');
      WriteLn('  remdir=/ruta/a/dir/remota');
      WriteLn('  rmhost=example.com');
      WriteLn('  person=usuario');
      WriteLn('  kaewor=secreto');
      WriteLn('Cada línea especifica una configuración. Las líneas que comienzan con # se tratan como comentarios y son ignoradas.');
    end;
    'pt':
    begin
      WriteLn('Uso: ugSFTP2 [opções]');
      WriteLn('Opções:');
      WriteLn('  -s, --server <endereço>       Especifica o endereço do servidor SFTP. Padrão: localhost');
      WriteLn('  -u, --user <usuário>          Especifica o nome de usuário para a conexão com o servidor. Padrão: user');
      WriteLn('  -p, --password <senha>        Especifica a senha para a conexão com o servidor.');
      WriteLn('  -l, --locdir <dir local>      Especifica o diretório local de onde os arquivos serão sincronizados.');
      WriteLn('  -r, --remdir <dir remoto>     Especifica o diretório remoto para onde os arquivos serão sincronizados.');
      WriteLn('  -c, --configfile <caminho do arquivo> Especifica um caminho alternativo para o arquivo de configuração.');
      WriteLn('  -h, --help                    Mostra esta mensagem de ajuda e sai.');
      WriteLn('  -h-??, --help-??              Mostra esta mensagem de ajuda em idioma ( ?? = de, fr, it, pt, es, ch, rt) e sai.');
      WriteLn;
      WriteLn('Descrição:');
      WriteLn('  ugSFTP2 sincroniza arquivos entre um diretório local e um remoto');
      WriteLn('  usando SFTP. Está projetado para gerenciar transferências de arquivos de maneira segura e');
      WriteLn('  eficiente. Você pode especificar todas as informações necessárias através de argumentos de linha de comando');
      WriteLn('  ou um arquivo de configuração, o que proporciona flexibilidade');
      WriteLn('  e facilidade de uso em scripts automatizados.');
      WriteLn;
      WriteLn('Exemplos:');
      WriteLn('  ugSFTP2 --server example.com --user meuusuario --password minhasenha --locdir /dir/local --remdir /dir/remoto');
      WriteLn('  ugSFTP2 -c /caminho/para/config.txt');
      WriteLn('  Para mais informações sobre o formato do arquivo de configuração, consulte o manual do usuário.');
      WriteLn;
      WriteLn('O arquivo de configuração deve ser um arquivo de texto simples com pares chave=valor em linhas separadas.');
      WriteLn('Exemplo de arquivo de configuração:');
      WriteLn('  locdir=/caminho/para/dir/local');
      WriteLn('  remdir=/caminho/para/dir/remoto');
      WriteLn('  rmhost=example.com');
      WriteLn('  person=usuario');
      WriteLn('  kaewor=segredo');
      WriteLn('Cada linha especifica uma configuração. Linhas que começam com # são tratadas como comentários e ignoradas.');
    end;
    'ch': // Alemannic help (Swiss-German)
    begin
      WriteLn('Bruuch: ugSFTP2 [Optione]');
      WriteLn('Optione:');
      WriteLn('  -s, --server <Adrässe>       Setzd d''Adrässe vom SFTP-Server. Standardmässig: localhost');
      WriteLn('  -u, --user <Benutzername>    Setzd dr Benutzername für d''Verbindig zum Server. Standardmässig: user');
      WriteLn('  -p, --password <Passwort>    Setzd s''Passwort für d''Verbindig zum Server.');
      WriteLn('  -l, --locdir <lokals Verzeichnis> Gsetz s''lokale Verzeichnis, vo wo d''Dateie synchronisiert wäre.');
      WriteLn('  -r, --remdir <entfernts Verzeichnis> Gsetz s''entfernte Verzeichnis, wo hiä d''Dateie synchronisiert wäre.');
      WriteLn('  -c, --configfile <Dateipfad> Setzd en alternative Pfad für s''Konfigurationsfile.');
      WriteLn('  -h, --help                   Zeigt dä Hilfetext aa und beendet s''Programm.');
      WriteLn('  -h-??, --help-??             Zeigt dä Hilfetext in dr gwünschte Sproch aa ( ?? = de, fr, it, pt, es, ch, rt) und beendet s''Programm.');
      WriteLn;
      WriteLn('Beschriibig:');
      WriteLn('  ugSFTP2 synchronisiert Dateie zwüsche me lokale und eme entfernte Verzeichnis');
      WriteLn('  under Verwändig vo SFTP. S''isch dezue da, Dateitransfere sicher und');
      WriteLn('  effizient z''handhabe. Du chasch alli notwändige Informatione über Befehlsziileargument');
      WriteLn('  oder es Konfigurationsfile aagäh, was Flexibilität');
      WriteLn('  und eifachi Verwändig in automatisierte Skripte ermöglicht.');
      WriteLn;
      WriteLn('Bischpel:');
      WriteLn('  ugSFTP2 --server example.com --user myuser --password mypass --locdir /local/dir --remdir /remote/dir');
      WriteLn('  ugSFTP2 -c /pfad/zum/konfig.txt');
      WriteLn('  Für meh Informatione zum Format vom Konfigurationsfile, lueg s''Benutzerhandbuech aa.');
      WriteLn;
      WriteLn('S''Konfigurationsfile sött e eifachs Textfile sii mit Schlüssel=Wert-Paar uf separate Ziile.');
      WriteLn('Bischpel für es Konfigurationsfile:');
      WriteLn('  locdir=/pfad/zum/lokale/verzeichnis');
      WriteLn('  remdir=/pfad/zum/entfernte/verzeichnis');
      WriteLn('  rmhost=example.com');
      WriteLn('  person=benutzername');
      WriteLn('  kaewor=geheimnis');
      WriteLn('Jedi Ziile spezifiziert e Istellig. Ziile wo mit # aafange, wäre als Kommentar behandlet und ignoriert.');
    end;
    'rt': // Romansh help
    begin
      WriteLn('Utilisaziun: ugSFTP2 [opziuns]');
      WriteLn('Opziuns:');
      WriteLn('  -s, --server <adressa>       Specifitgescha l''adressa dal server SFTP. Predefinit: localhost');
      WriteLn('  -u, --user <utilisader>      Specifitgescha il num d''utilisader per la colliaziun al server. Predefinit: user');
      WriteLn('  -p, --password <pled-clav>   Specifitgescha il pled-clav per la colliaziun al server.');
      WriteLn('  -l, --locdir <dir locala>    Specifitgescha la direcziun locala nua che ils files vegnan sincronisads.');
      WriteLn('  -r, --remdir <dir remota>    Specifitgescha la direcziun remota nua che ils files vegnan sincronisads.');
      WriteLn('  -c, --configfile <via dal datoteca> Specifitgescha ina via alternativa per il datoteca da configuraziun.');
      WriteLn('  -h, --help                   Mussa quest messadi d''agid ed sorta.');
      WriteLn('  -h-??, --help-??             Mussa quest messadi d''agid en la lingua specificada ( ?? = de, fr, it, pt, es, ch, rt) ed sorta.');
      WriteLn;
      WriteLn('Descripziun:');
      WriteLn('  ugSFTP2 sincronisescha datotecas tranter ina direcziun locala ed ina remota');
      WriteLn('  utilisond SFTP. El è concepì per manischar transfers da datotecas a moda segira ed');
      WriteLn('  effizienta. Vus pudais specifitgar tut las infurmaziuns necessarias tras arguments');
      WriteLn('  da lingia da cumond u in datoteca da configuraziun, quai che porscha flexibilitad');
      WriteLn('  ed utilisaziun simpla en scripts automatizads.');
      WriteLn;
      WriteLn('Exempels:');
      WriteLn('  ugSFTP2 --server example.com --user miuutilisader --password miapledclav --locdir /dir/locala --remdir /dir/remota');
      WriteLn('  ugSFTP2 -c /via/als/datotecas/config.txt');
      WriteLn('  Per dapli infurmaziuns davart il format dal datoteca da configuraziun, consultai il manual d''utilisader.');
      WriteLn;
      WriteLn('Il datoteca da configuraziun duai esser ina datoteca da text simpla cun pèra clav=valur sin lingias separadas.');
      WriteLn('Exempel dal datoteca da configuraziun:');
      WriteLn('  locdir=/via/alla/dir/locala');
      WriteLn('  remdir=/via/alla/dir/remota');
      WriteLn('  rmhost=example.com');
      WriteLn('  person=utilisader');
      WriteLn('  kaewor=secret');
      WriteLn('Mintga lingia specifitgescha ina configuraziun. Lingias che cumenzan cun # vegnan tractadas sco commentaris ed ignoradas.');
    end;
  else
    WriteLn('Unsupported language. Please use -h or --help for English.');
  end;
end;

end.
