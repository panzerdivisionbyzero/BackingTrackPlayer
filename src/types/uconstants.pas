{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UConstants;

{$mode objfpc}{$H+}

interface

const
  {$if defined(windows)}
    dirSeparator='\';
    rootDir='';
  {$else}
    dirSeparator='/';
    rootDir='/';
  {$endif}

  // screens definitions ids:
  sdTest=0;
  sdDirExplorer=1;
  sdSetlist=2;
  sdPlayer=3;
  sdSettings=4;
  sdCommandLine=5;
  sdKeyboard=6;
  // screen processes ids: // wspomaga okreslenie w jakim trybie ma byc uruchomiony nastepny ekran
  spNone=0;
  spPickTrack=1;
  spTypeNewSetlistFileName=2;
  spTypeTrackName=3;
  spTypeCommand=4;

  playerActive=True;

implementation

end.

