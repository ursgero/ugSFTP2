program ugSFTP2;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, ugSFTPunit, DateUtils;

type
  TConfig = record
    LocDir: string;
    RemDir: string;
    Server: string;
    User: string;
    Password: string;
  end;

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


  function ListRemoteFiles(SFTPClient: TugSFTP; const RemoteDir: string; FileList: TStringList): Boolean;
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
          FileList.Add(FileName + '=' + IntToStr(UnixTime));
        finally
          Temp.Free;
        end;
      end;

      Result := True;
    finally
      RemoteFileInfos.Free;
    end;
  end;



  procedure ListLocalFiles(const Dir: string; FileList: TStringList);
  var
    SR: TSearchRec;
    UnixTime: Int64;
    DateTime: TDateTime;
  begin
    if FindFirst(Dir + '*.*', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Attr and faDirectory) = 0 then
        begin
          DateTime := FileDateToDateTime(SR.Time);
          UnixTime := DateTimeToUnix(DateTime);
          WriteLn('Local file: ', SR.Name, ' DateTime: ', DateTimeToStr(DateTime), ' UnixTime: ', UnixTime);
          FileList.Add(SR.Name + '=' + IntToStr(UnixTime));
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;

  procedure SyncFiles(const Config: TConfig);
  var
    SFTPClient: TugSFTP;
    LocalFiles, RemoteFiles: TStringList;
    LocalFileName: string;
    i: Integer;
    LocalMTime, RemoteMTime: TDateTime;
  begin
    // Erstellen und Initialisieren des SFTP-Clients
    SFTPClient := TugSFTP.Create(PChar(Config.Server), 22, PChar(Config.User), PChar(Config.Password));
    try
      if SFTPClient.Connect then
      begin
        WriteLn('Connected to SFTP server.');

        // Initialisieren der TStringList-Instanzen
        LocalFiles := TStringList.Create;
        RemoteFiles := TStringList.Create;
        try
          // Lokale Dateien auflisten
          ListLocalFiles(Config.LocDir, LocalFiles);

          // Remote-Dateien auflisten
          if not ListRemoteFiles(SFTPClient, Config.RemDir, RemoteFiles) then
          begin
            WriteLn('Failed to list remote files.');
            Exit;
          end;

          // Vergleichen der Dateien und Hochladen der neueren Dateien
          for i := 0 to LocalFiles.Count - 1 do
          begin
            LocalFileName := LocalFiles.Names[i];
            LocalMTime := UnixTimeToDateTime(StrToInt64(LocalFiles.ValueFromIndex[i])); // Konvertierung der Unix-Zeit in TDateTime

            if RemoteFiles.IndexOfName(LocalFileName) <> -1 then
            begin
              RemoteMTime := UnixTimeToDateTime(StrToInt64(RemoteFiles.Values[LocalFileName]));
              WriteLn('Comparing: ', LocalFileName);
              WriteLn('Local modification time: ', DateTimeToStr(LocalMTime));
              WriteLn('Remote modification time: ', DateTimeToStr(RemoteMTime));

              if LocalMTime > RemoteMTime then
              begin
                WriteLn('Uploading ', LocalFileName);
                SFTPClient.UploadFile(PChar(Config.LocDir + LocalFileName), PChar(Config.RemDir + LocalFileName));
              end
              else
                WriteLn('Skipping ', LocalFileName);
            end
            else
            begin
              WriteLn('Uploading ', LocalFileName, ' (new file)');
              SFTPClient.UploadFile(PChar(Config.LocDir + LocalFileName), PChar(Config.RemDir + LocalFileName));
            end;
          end;

        finally
          LocalFiles.Free;
          RemoteFiles.Free;
        end;

      end
      else
        WriteLn('Failed to connect to SFTP server.');
    finally
      SFTPClient.Free;
    end;
  end;


var
  Config: TConfig;
begin
  Config := LoadConfig('/etc/ugftp/control.txt');
  SyncFiles(Config);
end.
