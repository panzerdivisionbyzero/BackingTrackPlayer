# About the project
The RPI Backing Track Player is designed for playing audio files (WAV, MP3) on Raspberry Pi with touch screen. It also displays Guitar Pro 4 tabulature if attached to MP3/WAV file.

Presentation on YouTube: https://www.youtube.com/watch?v=CkRdoIEv1PI

# Features:
- file explorer
- playing WAV and MP3 files
- volume adjustment (separated for left and right channel)
- tempo adjustment
- displaying tabulature compatible with Guitar Pro 4.06 file structure (auto-detect by WAV/MP3 file name)
- moving tabulature cursor (by measures and beats or tapping on tab)
- defining loop/exclusion region
- M3U playlists support
- access to commandline (both Linux and Windows) via touch keyboard

# License
Licensed under the terms of the GNU GPL 2.0 license, excluding used libraries:
- United Openlibraries of Sound (uos) licensed under LGPL 2.1;

and used code snippets marked with link to original source.	

Copyright (c) 2018-2022 by Pawel Witkowski

In case of questions contact me at: pawel.vitek.witkowski@gmail.com

# Is it work well
Needs some fixes to run perfectly, but generally works stable. Used with no worries live (drummer here) and at the recording session.

Written in Lazarus, compiled on Raspbian and Windows 8.1;

Tested on set:
- Raspberry Pi 2 B
- HiFiBerry DAC+ RCA 
- LCD touch screen 7" (1024x600)

# About the code
## Contents:
- [Main principles](#main-principles)
- [Class hierarchy](#class-hierarchy)
- [Directories hierarchy](#directories-hierarchy)
- [Main files description](#main-files-description)
- [Screens conception description](#screens-conception-description)
- [Rendering](#rendering)

## Main principles:
1. most often I use naming rule as follows: UClassName.pas (file) -> TClassName (type) -> classInstance:TClassName (instance:type declaration); e.g. object mainController is an instance of TMainController class, located in UMainController.pas;
2. Object Pascal inheritance syntax: class=TChildClass(TParentClass)

## Class hierarchy:
```
- frmMain:TFrmMain:
  - mc:TMainController:
    - ic:TInputController;
	- canvasBmp:TBitmap; // canvas buffer
	- <screens instances e.g. screenDirExplorer> // screens are kind of GUI pages with components; each screen inherits from TSimpleComponent class
	  // <each screen contains some components>:
	    - TSimpleLabel      (TSimpleComponent); // also used by other components e.g. TSimpleButton
	    - TSimpleButton     (TSimpleComponent);
		- TSimpleCheckbox   (TSimpleComponent);
		- TSimpleListBox    (TSimpleComponent);
		- TSimpleMarkersLine(TSimpleComponent); // refers to player loop marker
		- TSimpleScrollbar  (TSimpleComponent);
```
		
## Directories hierarchy:
```
\                         // main Lazarus project files and executable
\lib\                     // Lazarus and UOS libraries
\src\                     // *.pas source files 
\src\gpTabModule\         // Guitar Pro Tabulature module
\src\gpTabModule\helpers\ // gpTabModule helper classes, functions, types
\src\gpTabModule\modules\ // gpTabModule main source files
\src\gpTabModule\types\   // gpTabModule structures declarations
\src\screens\             // GUI pages classes
\src\simpleComponents\    // components classes
\src\tools\               // specialized classes like TPlayer or TDirExplorer
\src\types\               // global structures declarations
```

## Main files description:

[**btPlayer.lpr**](btPlayer.lpr) - main/initial unit (as always in Lazarus/Delphi), initializes app form: frmMain (UFrmMain.pas);

[**ufrmmain.pas**](src/ufrmmain.pas) - TFrmMain class unit; frmMain:TFrmMain instance responsibilities:
- contains and initializes mc:TMainController (implemented in UMainController);
- receives mouse (touch) events and passes them to the input controller (mc.ic) for further analysis;
- displays rendered image on pb:TPaintbox control;

[**umaincontroller.pas**](src/umaincontroller.pas) - TMainController classs unit; mainController:TMainController instance responsibilities:
- contains and initializes ic:TInputController;
- performs user actions processed (interpreted) by ic:TInputController and passes them to the active screen;
- contains canvas buffer (canvasBmp) and main Repaint() method;
- contains, initializes and switches "screens" (pages like file explorer view or player view);
- gives screens access to general methods via events, e.g. OnRepaintRequest() or OnSwitchToScreen();

## Screens conception description:
- the "screens" are full-screen pages with components (e.g. file explorer view or player view);
- all screens are equally stored in mc:TMainController as separated instances (screenDirExplorer, screenPlayer, etc...); they do not "know" about each other;
- mc:TMainController also stores an array with pointers to these instances (mc.pScreens[]) which makes easier to call them by indexes instead identifiers;
- switching and data exchange between screens:
  - centrally handled by TMainController.SwitchToScreen();
  - screens can call this method by calling event OnSwitchToScreen();
  - each screen.OnSwitchToScreen() is linked to TMainController.SwitchToScreen() method after screen initialization in TMainController.PrepareScreens():
    - example: screenPlayer.OnSwitchToScreen:=@SwitchToScreen;

## Rendering:
**Drawing BTP components has 2 phases:**
1. Render() - in this step the image of component is being drawn on component's imgBuf:TBitmap buffer; Calling Render() is necessary only when the component changes state and it's triggered by component methods (e.g. component.SetSize());
2. Repaint() - pastes component.imgBuf with it's sub-components imgBuf buffers on given canvas (which is faster than repeating full Render());

**Calling render:**<br/>
- internal by component state change;

**Calling repaint:**<br/>
- main method: TMainController.Repaint(); due to the specificity of render output (frmMain.pb:TPaintBox), TMainController.Repaint() can be called by TfrmMain.pb.Repaint() only;
- TfrmMain.pb.Repaint() calls TMainController.Repaint() and passes destination canvas in parameter; TfrmMain.pb.Repaint() can be triggered by 2 ways:
  - by TfrmMain itself (e.g. by form mouse events);
  - by TMainController.OnRepaintRequest() (components repaint requests (e.g. player position change) allowed by TMainController.repaintRequestTimer (to limit requests frequency));
