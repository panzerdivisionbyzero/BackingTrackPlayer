{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UKeyboardScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  Graphics,
  Dialogs, Forms,
  ExtCtrls,
  UConstants,
  UUtilities,
  USimpleComponent,
  USimpleButton,
  USimpleLabel,
  USimpleScrollbar,
  USimpleCheckbox,
  USimpleListbox;

type
  // pages -> rows -> individual keys
  TKeyState=record
    width:integer;
    row:byte;
    symbols:array [0..3] of string;
  end;

const
  keysDefs:array [0..34] of TKeyState = (
    // *** state 0:
    // qwertyuiop
    // asdfghjkl
    // zxcvbnm
    // *** state 1:
    // QWERTYUIOP
    // ASDFGHJKL
    // ZXCVBNM
    // *** state 2:
    // 1234567890
    // !@#$%^&()
    // '",.?\/
    // *** state 3:
    // 1234567890
    // ~*+-=[]{}
    // `_|;:<>
    (width: 96; row:0; symbols:('q','Q','1','1')),
    (width: 96; row:0; symbols:('w','W','2','2')),
    (width: 96; row:0; symbols:('e','E','3','3')),
    (width: 96; row:0; symbols:('r','R','4','4')),
    (width: 96; row:0; symbols:('t','T','5','5')),
    (width: 96; row:0; symbols:('y','Y','6','6')),
    (width: 96; row:0; symbols:('u','U','7','7')),
    (width: 96; row:0; symbols:('i','I','8','8')),
    (width: 96; row:0; symbols:('o','O','9','9')),
    (width: 96; row:0; symbols:('p','P','0','0')),
    (width: 96; row:1; symbols:('a','A','!','~')),
    (width: 96; row:1; symbols:('s','S','@','*')),
    (width: 96; row:1; symbols:('d','D','#','+')),
    (width: 96; row:1; symbols:('f','F','$','-')),
    (width: 96; row:1; symbols:('g','G','%','=')),
    (width: 96; row:1; symbols:('h','H','^','[')),
    (width: 96; row:1; symbols:('j','J','&',']')),
    (width: 96; row:1; symbols:('k','K','(','{')),
    (width: 96; row:1; symbols:('l','L',')','}')),
    (width:128; row:2; symbols:('SHIFT','*SHIFT*','1/2','2/2')),
    (width: 96; row:2; symbols:('z','Z',#39,'`')),
    (width: 96; row:2; symbols:('x','X','"','_')),
    (width: 96; row:2; symbols:('c','C',',','|')),
    (width: 96; row:2; symbols:('v','V','.',';')),
    (width: 96; row:2; symbols:('b','B','?',':')),
    (width: 96; row:2; symbols:('n','N','\','<')),
    (width: 96; row:2; symbols:('m','M','/','>')),
    (width:128; row:2; symbols:('DEL','DEL','DEL','DEL')),
    (width:128; row:3; symbols:('123','123','ABC','ABC')),
    (width: 96; row:3; symbols:(dirSeparator,dirSeparator,dirSeparator,dirSeparator)),
    (width:320; row:3; symbols:(' ',' ',' ',' ')),
    (width: 96; row:3; symbols:('.','.','.','.')),
    (width: 96; row:3; symbols:('<-','<-','<-','<-')),
    (width: 96; row:3; symbols:('->','->','->','->')),
    (width:128; row:3; symbols:('ENTER','ENTER','ENTER','ENTER'))
  );

type
  TKbState=(kbsLowerCase,kbsUpperCase,kbsSpecials1,kbsSpecials2);
  TKeyboardScreen=class(TSimpleComponent)
    private
      faceColor:integer;
      resultText:string;

      btnExit:TSimpleButton;
      labResultText:TsimpleLabel;
      kbButtons:array [0..Length(keysDefs)-1] of TSimpleButton;
      kbState:TKbState;

      cursorVisible:boolean;
      cursorPos:integer;
      cursorTimer:TTimer;

      SwitchToScreenEvent:TSwitchToScreenEvent;
      RepaintRequestEvent:TRepaintRequestEvent;

      procedure RenderImgBuf; override;
      procedure PrepareComponents;
      procedure SetKbState(new_kbState:TKbState);
      procedure SetResultText(newResultText:string);
      procedure RemoveCharacter(pos:integer);
      procedure InsertText(pos:integer; text:string);
      procedure BtnExitClick(sender:TObject; x,y:integer);
      procedure BtnKeyboardClick(sender:TObject; x,y:integer);
      procedure CursorTimerTick(Sender:TObject);
    public
      parentScreenId:integer;
      screenProcessId:integer;
      constructor Create;
      destructor Destroy; override;
      procedure SetText(text:string);
      function GetText:string;
      procedure SetCursorPos(newCursorPos:integer);
      procedure Activate;
      procedure Deactivate;
      property OnSwitchToScreen:TSwitchToScreenEvent read SwitchToScreenEvent write SwitchToScreenEvent;
      property OnRepaintRequest:TRepaintRequestEvent read RepaintRequestEvent write RepaintRequestEvent;
      procedure TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
      procedure TriggerOnRepaintRequestEvent;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TKeyboardScreen.Create;
begin
  inherited;

  faceColor:=clBlack;
  resultText:='';
  parentScreenId:=sdDirExplorer;
  screenProcessId:=spNone;
  kbState:=kbsLowerCase;
  cursorVisible:=True;
  cursorPos:=0;

  cursorTimer:=TTimer.Create(nil);
  cursorTimer.OnTimer:=@CursorTimerTick;
  cursorTimer.Interval:=500;
  cursorTimer.Enabled:=False;

  PrepareComponents;
end;

destructor TKeyboardScreen.Destroy;
begin
  //

  inherited;
end;

procedure TKeyboardScreen.SetText(text:string);
begin
  cursorPos:=0;
  SetResultText(text);
end;

function TKeyboardScreen.GetText:string;
begin
  result:=resultText;
end;

procedure TKeyboardScreen.SetCursorPos(newCursorPos:integer);
begin
  cursorPos:=Max(0,Min(Length(resultText),newCursorPos));
  RenderImgBuf;
end;

procedure TKeyboardScreen.Activate;
begin
  cursorTimer.Enabled:=True;
end;

procedure TKeyboardScreen.Deactivate;
begin
  cursorTimer.Enabled:=False;
end;

procedure TKeyboardScreen.TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
begin
  if Assigned(OnSwitchToScreen) then OnSwitchToScreen(screenId, newScreenProcessId);
end;

procedure TKeyboardScreen.TriggerOnRepaintRequestEvent;
begin
  if Assigned(OnRepaintRequest) then OnRepaintRequest;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TKeyboardScreen.RenderImgBuf;
var labelCenterY,halfTextHeight,cursorLeft:integer;
begin
  if (fWidth=0) or (fHeight=0) then exit;

  imgBuf.canvas.Brush.Color:=faceColor;
  imgBuf.canvas.FillRect(0,0,fWidth,fHeight);

  if cursorVisible then
  begin
    labelCenterY:=labResultText.top+Round(labResultText.height*0.5);
    halfTextHeight:=Round(labResultText.textHeight*0.5);
    cursorLeft:=labResultText.left+labResultText.EstimateTextWidth(Copy(resultText,1,cursorPos));
    imgBuf.canvas.Pen.Color:=clYellow;
    imgBuf.canvas.Pen.Width:=2;
    imgBuf.canvas.Pen.Style:=psSolid;
    imgBuf.canvas.MoveTo(cursorLeft,labelCenterY-halfTextHeight);
    imgBuf.canvas.LineTo(cursorLeft,labelCenterY+halfTextHeight);
  end;
end;

procedure TKeyboardScreen.PrepareComponents;
const
  btnMargin=4;
  btnHeight=96;
  kbLeft=16;
  kbTop=128;
var i,k,col,rowLeft,rowWidth,btnLeft:integer;
begin
  btnExit:=TSimpleButton.Create;
  btnExit.SetCaption('X');
  btnExit.SetSize(96,96);
  btnExit.SetPos(1024-96-16,16);
  btnExit.OnMouseClick:=@BtnExitClick;
  RegisterComponent(@btnExit);

  labResultText:=TSimpleLabel.Create;
  labResultText.SetTransparency(True);
  labResultText.SetCaption(resultText);
  labResultText.SetSize(768,96);
  labResultText.SetPos(128,16);
  RegisterComponent(@labResultText);

  for i:=0 to High(kbButtons) do
  begin
    if (i=0)
      or ((i>0) and (keysDefs[i].row>keysDefs[i-1].row)) then
    begin
      col:=0;
      rowWidth:=-btnMargin+kbLeft;
      for k:=i to High(kbButtons) do
      begin
        if keysDefs[k].row<>keysDefs[i].row then break;
        rowWidth:=rowWidth+btnMargin+keysDefs[k].width;
      end;
      rowLeft:=Round((1024-rowWidth)*0.5);
      btnLeft:=rowLeft;
    end;

    kbButtons[i]:=TSimpleButton.Create;
    kbButtons[i].SetSize(keysDefs[i].width,btnHeight);
    kbButtons[i].SetPos(btnLeft,kbTop+keysDefs[i].row*(btnMargin+btnHeight));
    kbButtons[i].OnMouseClick:=@BtnKeyboardClick;
    RegisterComponent(@kbButtons[i]);

    col:=col+1;
    btnLeft:=btnLeft+btnMargin+keysDefs[i].width;
  end;

  SetKbState(kbsLowerCase);
end;

procedure TKeyboardScreen.SetKbState(new_kbState:TKbState);
var i,intKbState:integer;
begin
  kbState:=new_kbState;
  intKbState:=Integer(kbState);
  for i:=0 to High(kbButtons) do
  begin
    kbButtons[i].SetCaption(keysDefs[i].symbols[intKbState]);
  end;
end;

procedure TKeyboardScreen.SetResultText(newResultText:string);
begin
  resultText:=newResultText;
  labResultText.SetCaption(resultText);
end;

procedure TKeyboardScreen.RemoveCharacter(pos:integer);
begin
  if (cursorPos<=0) or (Length(resultText)=0) then exit;
  Delete(resultText,cursorPos,1);
  labResultText.SetCaption(resultText);
  if cursorPos>0 then
    cursorPos:=cursorPos-1;
end;

procedure TKeyboardScreen.InsertText(pos:integer; text:string);
begin
  Insert(text,resultText,cursorPos+1);
  labResultText.SetCaption(resultText);
  cursorPos:=cursorPos+1;
end;

procedure TKeyboardScreen.BtnExitClick(sender:TObject; x,y:integer);
begin
  if parentScreenId=-1 then TerminateApp
                       else TriggerOnSwitchToScreenEvent(parentScreenId);
end;

procedure TKeyboardScreen.BtnKeyboardClick(sender:TObject; x,y:integer);
var btnCaption:string;
begin
  btnCaption:=((sender as TSimpleButton).GetCaption);

  if btnCaption='SHIFT' then
    SetKbState(kbsUpperCase)
  else
  if btnCaption='*SHIFT*' then
    SetKbState(kbsLowerCase)
  else
  if btnCaption='1/2' then
    SetKbState(kbsSpecials2)
  else
  if btnCaption='2/2' then
    SetKbState(kbsSpecials1)
  else
  if btnCaption='123' then
    SetKbState(kbsSpecials1)
  else
  if btnCaption='ABC' then
    SetKbState(kbsLowerCase)
  else
  if btnCaption='DEL' then
    RemoveCharacter(cursorPos)
  else
  if btnCaption='<-' then
    begin if cursorPos>0 then cursorPos:=cursorPos-1; end
  else
  if btnCaption='->' then
    begin if cursorPos<Length(resultText) then cursorPos:=cursorPos+1; end
  else
  if btnCaption='ENTER' then
    case screenProcessId of
      spTypeNewSetlistFileName: TriggerOnSwitchToScreenEvent(sdSetlist, spTypeNewSetlistFileName);
      spTypeTrackName  : TriggerOnSwitchToScreenEvent(sdSetlist, spTypeTrackName);
      spTypeCommand    : TriggerOnSwitchToScreenEvent(sdCommandLine, spTypeCommand);
    end
  else
    InsertText(cursorPos,btnCaption);

  cursorVisible:=True;
  cursorTimer.Enabled:=False;
  cursorTimer.Enabled:=True;
  RenderImgBuf;
end;

procedure TKeyboardScreen.CursorTimerTick(Sender:TObject);
begin
  cursorVisible:=not cursorVisible;
  RenderImgBuf;
  TriggerOnRepaintRequestEvent;
end;

end.

