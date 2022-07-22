{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UDirExplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs,
  UConstants,USimpleListbox,USystemTools;

const
  dirPrefix='<DIR> ';

type
  TChangeDirectoryEvent=procedure (newDir:string) of object;
  TDirExplorer=class
    private
      fCurrentDir:string;
      fCurrentDirFiles:TStringList;
      fCurrentDirFolders:TStringList;
      fListbox:TSimpleListbox;

      ChangeDirectoryEvent:TChangeDirectoryEvent;

      procedure ListDirectory(globalDir:string; var files:TStringList; var folders:TStringList);
      procedure ListItemsToListbox;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AssignListbox(listbox:TSimpleListbox);
      procedure GoToDirectory(globalDir:string);
      procedure ChangeDirectory(localDir:string);
      property OnChangeDirectory:TChangeDirectoryEvent read ChangeDirectoryEvent write ChangeDirectoryEvent;
      procedure TriggerOnChangeDirectoryEvent(newDir:string);
    published
      property currentDir:string read fCurrentDir;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TDirExplorer.Create;
begin
  fCurrentDirFiles:=TStringList.Create;
  fCurrentDirFolders:=TStringList.Create;
  fListbox:=nil;
  fCurrentDir:=rootDir;
end;

destructor TDirExplorer.Destroy;
begin
  fCurrentDirFiles.Destroy;
  fCurrentDirFolders.Destroy;

  inherited;
end;

procedure TDirExplorer.AssignListbox(listbox:TSimpleListbox);
begin
  fListbox:=listbox;
end;

procedure TDirExplorer.GoToDirectory(globalDir:string);
begin
  fCurrentDir:=globalDir;
  if (globalDir=rootDir) then
  begin
    fCurrentDirFiles.Clear;
    ListDrives(fCurrentDirFolders);
  end
  else
  begin
    if fCurrentDir[Length(fCurrentDir)]=dirSeparator then
    fCurrentDir:=Copy(fCurrentDir,1,Length(fCurrentDir)-1);
    ListDirectory(fCurrentDir,fCurrentDirFiles,fCurrentDirFolders);
  end;
  ListItemsToListbox;
  TriggerOnChangeDirectoryEvent(fCurrentDir);
end;

procedure TDirExplorer.ChangeDirectory(localDir:string);
var newDir:string;
begin
  if Copy(localDir,1,Length(dirPrefix))=dirPrefix then
    localDir:=Copy(localDir,Length(dirPrefix)+1,Length(localDir)-Length(dirPrefix));
  if localDir='..' then
  begin
    newDir:=ExtractFilePath(fCurrentDir);
    if newDir=currentDir then
    begin
      GoToDirectory(rootDir);
      exit;
    end;
  end
  else
  if (fCurrentDir=rootDir) then
    newDir:=rootDir+localDir
  else
    newDir:=fCurrentDir+dirSeparator+localDir;

  if not DirectoryExists(newDir) then
  begin
    ShowMessage('Directory does not exist:'+#13+newDir);
    exit;
  end;
  GoToDirectory(newDir);
end;

procedure TDirExplorer.TriggerOnChangeDirectoryEvent(newDir:string);
begin
  if Assigned(OnChangeDirectory) then OnChangeDirectory(newDir);
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TDirExplorer.ListDirectory(globalDir:string; var files:TStringList; var folders:TStringList);
begin
  fCurrentDirFiles.Clear;
  fCurrentDirFolders.Clear;
  try
    FileUtil.FindAllFiles(files, globalDir, '*', False, faReadOnly);
    FileUtil.FindAllDirectories(folders, globalDir, False);
  except
    ShowMessage('Cannot list directory:'+#13+globalDir);
  end;
end;

procedure TDirExplorer.ListItemsToListbox;
var i:integer;
begin
  if not Assigned(fListbox) then exit;
  fListbox.ClearItems;

  if (currentDir=rootDir) then
  begin
    for i:=0 to fCurrentDirFolders.Count-1 do
      fListbox.AddItem('<DIR> '+fCurrentDirFolders.Strings[i]);
  end
  else
  begin
    fListbox.AddItem('<DIR> ..');
  for i:=0 to fCurrentDirFolders.Count-1 do
    fListbox.AddItem('<DIR> '+ExtractFileName(fCurrentDirFolders.Strings[i]));
  for i:=0 to fCurrentDirFiles.Count-1 do
    fListbox.AddItem(ExtractFileName(fCurrentDirFiles.Strings[i]));
  end;
end;

end.

