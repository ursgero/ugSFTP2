program ugSFTP2;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, ugSFTPunit, HelpTexts, DateUtils;

  type
    TConfig = record
      LocDir: string;
      RemDir: string;
      Server: string;
      User: string;
      Password: string;
      ConfigFile: string;
    end;

{
  function LoadConfig(const FileName: string): TConfig;
  var
    ConfigFile: TextFile;
    Line, Key, Value: string;
    Config: TConfig;
  begin
    AssignFile(ConfigFile, FileName);
    try
      Reset(ConfigFile);
    except
      on E: EInOutError do
      begin
        WriteLn('Error: Could not open configuration file: ', FileName);
        Halt(1);
      end;
    end;

    try
      while not EOF(ConfigFile) do
      begin
        ReadLn(ConfigFile, Line);
        if Pos('=', Line) > 0 then
        begin
          Key := Trim(Copy(Line, 1, Pos('=', Line) - 1));
          Value := Trim(Copy(Line, Pos('=', Line) + 1, Length(Line)));
          if Key = 'locdir' then
            Config.LocDir := Value
          else if Key = 'remdir' then
            Config.RemDir := Value
          else if Key = 'rmhost' then
            Config.Server := Value
          else if Key = 'person' then
            Config.User := Value
          else if Key = 'kaewor' then
            Config.Password := Value;
        end;
      end;
    finally
      CloseFile(ConfigFile);
    end;
    Result := Config;
  end;

  }

  function DateTimeToUnix(DateTime: TDateTime): Int64;
  const
    UNIX_EPOCH: TDateTime = 25569.0; // 1. Januar 1970
  begin
    Result := Round((DateTime - UNIX_EPOCH) * 86400.0);
  end;

  function UnixTimeToDateTime(const UnixTime: Int64): TDateTime;
  const
    UNIX_EPOCH: TDateTime = 25569.0; // 1. Januar 1970 in TDateTime
    SECONDS_IN_DAY = 86400.0;        // Anzahl der Sekunden in einem Tag
  begin
    // Unix-Zeitstempel basiert auf der Anzahl der Sekunden seit dem 01.01.1970
    Result := (UnixTime / SECONDS_IN_DAY) + UNIX_EPOCH;
  end;


  function ListRemoteFiles(SFTPClient: TugSFTP; const RemoteDir: string; FileListA,FileListB: TStringList): Boolean;
  var
    RemoteFileInfos: TStringList;
    Temp: TStringList;
    i: Integer;
    FileName: string;
    UnixTime: Int64;
    PRemoteDir: PChar;
  begin
    RemoteFileInfos := TStringList.Create;
    try
      PRemoteDir := PChar(RemoteDir); // Umwandeln des Strings in PChar
      // Rufen Sie die Liste der Remote-Dateien ab
      if not SFTPClient.ListFiles(PRemoteDir, RemoteFileInfos) then
      begin
        WriteLn('Error: Unable to list files in remote directory: ', RemoteDir);
        Exit(False);
      end;

      // Verarbeiten der zurückgegebenen Dateiinformationen
      for i := 0 to RemoteFileInfos.Count - 1 do
      begin
        writeln(RemoteFileInfos[i]);
        Temp := TStringList.Create;
        try
          Temp.Delimiter := ';';
          Temp.DelimitedText := RemoteFileInfos[i];
          Temp.NameValueSeparator := '=';
          Temp.DelimitedText := StringReplace(RemoteFileInfos[i], '"', '', [rfReplaceAll]);
//          writeln('Parsed Text: ', Temp.Text);
//          writeln('Name: ', Temp.Values['Name']);
//          writeln('mdate: ', Temp.Values['mdate']);
          FileName := Temp.Values['Name'];
          UnixTime := StrToInt64(Temp.Values['mdate']);
          // Fügen Sie die Information zur FileList hinzu
          FileListA.Add(FileName + '=' + IntToStr(UnixTime));
          FileListB.Add(FileName + '=' + Temp.Values['Groesse']);
        finally
          Temp.Free;
        end;
      end;

      Result := True;
    finally
      RemoteFileInfos.Free;
    end;
  end;


  procedure ListLocalFiles(const Dir: string; FileListA, FileListB: TStringList);
  var
    SR: TSearchRec;
    UnixTime: Int64;
    DateTime: TDateTime;
    FileSize: Int64;
  begin
    if FindFirst(Dir + '*.*', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Attr and faDirectory) = 0 then
        begin
          // Berechnung von Datum und Zeit in Unix-Zeitstempel
          DateTime := FileDateToDateTime(SR.Time);
          UnixTime := DateTimeToUnix(DateTime);
          // Ausgabe zur Überprüfung
          WriteLn('Local file: ', SR.Name, ' DateTime: ', DateTimeToStr(DateTime), ' UnixTime: ', UnixTime);

          // Hinzufügen des Namens und des Alters zur FileListA
          FileListA.Add(SR.Name + '=' + IntToStr(UnixTime));

          // Berechnung der Dateigröße
          FileSize := SR.Size;
          // Hinzufügen des Namens und der Größe zur FileListB
          FileListB.Add(SR.Name + '=' + IntToStr(FileSize));
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;


  procedure SyncFiles(const Config: TConfig);
  var
    SFTPClient: TugSFTP;
    LocalFilesA, LocalFilesB, RemoteFilesA, RemoteFilesB: TStringList;
    LocalFileName: string;
    i: Integer;
    LocalMTime, RemoteMTime: TDateTime;
    LocalSize, RemoteSize: Int64;
  begin
    // Erstellen und Initialisieren des SFTP-Clients
    SFTPClient := TugSFTP.Create(PChar(Config.Server), 22, PChar(Config.User), PChar(Config.Password));
    try
      if SFTPClient.Connect then
      begin
        WriteLn('Connected to SFTP server.');

        // Initialisieren der TStringList-Instanzen
        LocalFilesA := TStringList.Create;  // Liste für lokale Datei-Alter
        LocalFilesB := TStringList.Create;  // Liste für lokale Datei-Größen
        RemoteFilesA := TStringList.Create; // Liste für entfernte Datei-Alter
        RemoteFilesB := TStringList.Create; // Liste für entfernte Datei-Größen
        try
          // Lokale Dateien auflisten
          ListLocalFiles(Config.LocDir, LocalFilesA, LocalFilesB);

          // Remote-Dateien auflisten
          if not ListRemoteFiles(SFTPClient, Config.RemDir, RemoteFilesA, RemoteFilesB) then
          begin
            WriteLn('Failed to list remote files.');
            Exit;
          end;

          // Vergleichen der Dateien und Hochladen der neueren oder größeren Dateien
          for i := 0 to LocalFilesA.Count - 1 do
          begin
            LocalFileName := LocalFilesA.Names[i];
            LocalMTime := UnixTimeToDateTime(StrToInt64(LocalFilesA.Values[LocalFileName]));
            LocalSize := StrToInt64(LocalFilesB.Values[LocalFileName]);

            if RemoteFilesA.IndexOfName(LocalFileName) <> -1 then
            begin
              RemoteMTime := UnixTimeToDateTime(StrToInt64(RemoteFilesA.Values[LocalFileName]));
              RemoteSize := StrToInt64(RemoteFilesB.Values[LocalFileName]);

              WriteLn('Comparing: ', LocalFileName);
              WriteLn('Local modification time: ', DateTimeToStr(LocalMTime), ' Remote modification time: ', DateTimeToStr(RemoteMTime));
              WriteLn('Local size: ', LocalSize, ' Remote size: ', RemoteSize);

              if (LocalMTime > RemoteMTime) or (LocalSize > RemoteSize) then
              begin
                WriteLn('Uploading ', LocalFileName);
                SFTPClient.UploadFile(PChar(Config.LocDir + '/' + LocalFileName), PChar(Config.RemDir + '/' + LocalFileName));
              end
              else
                WriteLn('Skipping ', LocalFileName);
            end
            else
            begin
              WriteLn('Uploading ', LocalFileName, ' (new file)');
              SFTPClient.UploadFile(PChar(Config.LocDir + '/' + LocalFileName), PChar(Config.RemDir + '/' + LocalFileName));
            end;
          end;

        finally
          LocalFilesA.Free;
          LocalFilesB.Free;
          RemoteFilesA.Free;
          RemoteFilesB.Free;
        end;

      end
      else
        WriteLn('Failed to connect to SFTP server.');
    finally
      SFTPClient.Free;
    end;
  end;

  function LoadConfig(const FileName: string): TConfig;
  var
    ConfigFile: TextFile;
    Line, Key, Value: string;
  begin
    AssignFile(ConfigFile, FileName);
    try
      Reset(ConfigFile);
      while not EOF(ConfigFile) do
      begin
        ReadLn(ConfigFile, Line);
        if Pos('=', Line) > 0 then
        begin
          Key := Trim(Copy(Line, 1, Pos('=', Line) - 1));
          Value := Trim(Copy(Line, Pos('=', Line) + 1, Length(Line)));
          if Key = 'locdir' then
            Result.LocDir := Value
          else if Key = 'remdir' then
            Result.RemDir := Value
          else if Key = 'server' then
            Result.Server := Value
          else if Key = 'user' then
            Result.User := Value
          else if Key = 'password' then
            Result.Password := Value;
        end;
      end;
    finally
      CloseFile(ConfigFile);
    end;
  end;

  procedure ParseCommandLine(var Config: TConfig);
  var
    i: Integer;
  begin
    i := 1;
    while i <= ParamCount do
    begin
      if (ParamStr(i) = '--server') or (ParamStr(i) = '-s') then
      begin
        Inc(i);
        if i <= ParamCount then
          Config.Server := ParamStr(i);
      end
      else if (ParamStr(i) = '--user') or (ParamStr(i) = '-u') then
      begin
        Inc(i);
        if i <= ParamCount then
          Config.User := ParamStr(i);
      end
      else if (ParamStr(i) = '--password')  or (ParamStr(i) = '-p') then
      begin
        Inc(i);
        if i <= ParamCount then
          Config.Password := ParamStr(i);
      end
      else if (ParamStr(i) = '--locdir')  or (ParamStr(i) = '-l') then
      begin
        Inc(i);
        if i <= ParamCount then
          Config.LocDir := ParamStr(i);
      end
      else if (ParamStr(i) = '--remdir')  or (ParamStr(i) = '-r') then
      begin
        Inc(i);
        if i <= ParamCount then
          Config.RemDir := ParamStr(i);
      end
      else if (ParamStr(i) = '--configfile') or (ParamStr(i) = '-c') then
      begin
        Inc(i);
        if i <= ParamCount then
          Config.ConfigFile := ParamStr(i);
      end
      else begin
        if (ParamStr(i) = '-h') or (ParamStr(i) = '--help') then
           ShowHelp('en')
        else
        if (ParamStr(i) = '--help-en') or (ParamStr(i) = '-h-en') then
          ShowHelp('en')
        else
        if (ParamStr(i) = '--help-de') or (ParamStr(i) = '-h-de') then
          ShowHelp('de')
        else
        if (ParamStr(i) = '--help-fr') or (ParamStr(i) = '-h-fr') then
          ShowHelp('fr')
        else
        if (ParamStr(i) = '--help-it') or (ParamStr(i) = '-h-it') then
         ShowHelp('it')
        else
        if (ParamStr(i) = '--help-es') or (ParamStr(i) = '-h-es') then
          ShowHelp('es')
        else
        if (ParamStr(i) = '--help-pt') or (ParamStr(i) = '-h-pt') then
            ShowHelp('pt')
        else
        if (ParamStr(i) = '--help-ch') or (ParamStr(i) = '-h-ch') then
          ShowHelp('ch')
        else
        if (ParamStr(i) = '--help-rt') or (ParamStr(i) = '-h-rt') then
            ShowHelp('rt');
        Halt(0);  // Exit program after showing help
      end;
      Inc(i);
//      end;
    end;
  end;

var
  Config: TConfig;

begin

// Set default values for the config
  Config.Server := 'localhost';
  Config.User := 'user';
  Config.Password := 'password';
  Config.LocDir := '/';
  Config.RemDir := '/';
  Config.ConfigFile := '/etc/ugftp/control.txt'; // Default configuration file
  // Parse command line arguments first

  ParseCommandLine(Config);

  // Load configuration from specified file

  Config := LoadConfig(Config.ConfigFile);

//  Config := LoadConfig('/etc/ugftp/control.txt');

  SyncFiles(Config);
end.
