unit ugSFTPunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TugSFTP = class
  private
    FClient: Pointer;
  public
    constructor Create(const Host: PChar; Port: Integer; const Username, Password: PChar);
    destructor Destroy; override;
    function Connect: Boolean;
    function ListFiles(const Directory: PChar; FileList: TStringList): Boolean;
    function UploadFile(const LocalPath, RemotePath: PChar): Boolean;
    function DownloadFile(const RemotePath, LocalPath: PChar): Boolean;
  end;

implementation

const
  UgSFTPClientLib = 'lib/libugsftpclient.so';

function UgSFTPClientCreate(Host: PChar; Port: Integer; Username, Password: PChar): Pointer; cdecl; external UgSFTPClientLib;
procedure UgSFTPClientDestroy(Client: Pointer); cdecl; external UgSFTPClientLib;
function UgSFTPClientConnect(Client: Pointer): Boolean; cdecl; external UgSFTPClientLib;
function UgSFTPClientListFiles(Client: Pointer; Directory: PChar): PChar; cdecl; external UgSFTPClientLib; // Updated return type
procedure UgSFTPClientFreeString(Str: PChar); cdecl; external UgSFTPClientLib; // New procedure for freeing strings
function UgSFTPClientUploadFile(Client: Pointer; LocalPath, RemotePath: PChar): Boolean; cdecl; external UgSFTPClientLib;
function UgSFTPClientDownloadFile(Client: Pointer; RemotePath, LocalPath: PChar): Boolean; cdecl; external UgSFTPClientLib;

constructor TugSFTP.Create(const Host: PChar; Port: Integer; const Username, Password: PChar);
begin
  inherited Create;
  FClient := UgSFTPClientCreate(Host, Port, Username, Password);
end;

destructor TugSFTP.Destroy;
begin
  if FClient <> nil then
    UgSFTPClientDestroy(FClient);
  inherited Destroy;
end;

function TugSFTP.Connect: Boolean;
begin
  Result := UgSFTPClientConnect(FClient);
end;

function TugSFTP.ListFiles(const Directory: PChar; FileList: TStringList): Boolean;
var
  FileInfoString: PChar;
begin
  FileInfoString := UgSFTPClientListFiles(FClient, Directory);
  if FileInfoString = nil then
  begin
    WriteLn('Error: Unable to list files in remote directory: ', Directory);
    Exit(False);
  end;

  try
    FileList.Text := StringReplace(StrPas(FileInfoString), ',', #13#10, [rfReplaceAll]);
    Result := True;
  finally
    UgSFTPClientFreeString(FileInfoString);
  end;
end;

function TugSFTP.UploadFile(const LocalPath, RemotePath: PChar): Boolean;
begin
  Result := UgSFTPClientUploadFile(FClient, LocalPath, RemotePath);
end;

function TugSFTP.DownloadFile(const RemotePath, LocalPath: PChar): Boolean;
begin
  Result := UgSFTPClientDownloadFile(FClient, RemotePath, LocalPath);
end;

end.
