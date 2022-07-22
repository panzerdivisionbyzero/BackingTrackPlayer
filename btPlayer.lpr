{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}

program btPlayer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UfrmMain, uMainController, USimpleButton, UConstants,
  USimpleComponent, UTestScreen, USimpleLabel, USimpleScrollbar,
  USimpleCheckbox, UInputController, USimpleListBox, UUtilities, UDirExplorer,
  UDirExplorerScreen, USetlistScreen, UPlayerScreen, UPlayer, USettingsScreen,
  USimpleMarkersLine, UKeyboardScreen, USystemTools, UCommandLineScreen,
  UGPTabModule, UIntByteSizeStrings, UEventSuspender, ugptabtimeutils,
  UGPTabFileReader, UGPTabDisplay, UGPTabModuleTypes, uGPTabFileStructureTypes;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

