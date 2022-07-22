{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UfrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Types,
  UMainController;

type
  TfrmMain = class(TForm)
    pb: TPaintBox;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure pbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbPaint(Sender: TObject);
  private
    mc:TMainController;
    procedure RepaintRequestEvent;

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  mc:=TMainController.Create;
  mc.OnRepaintRequest:=@RepaintRequestEvent;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  left:=0;
  top:=0;
  width:=1024;
  height:=600;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  mc.Destroy;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  //
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  mc.SetScreenSize(width,height);
  pb.Repaint;
end;

procedure TfrmMain.pbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mc.ic.MouseDown(x,y);
  pb.Repaint;
end;

procedure TfrmMain.pbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mc.ic.MouseMove(x,y);
  pb.Repaint;
end;

procedure TfrmMain.pbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mc.ic.MouseUp(x,y);
  pb.Repaint;
end;

procedure TfrmMain.pbPaint(Sender: TObject);
begin
  mc.Repaint(@pb.Canvas);
end;

procedure TfrmMain.RepaintRequestEvent;
begin
  pb.Repaint;
end;

end.

