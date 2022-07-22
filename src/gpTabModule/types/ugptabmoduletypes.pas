{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UGPTabModuleTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  UGPTabFileStructureTypes;

const
  // tuplets: 3,(4),5,6,7,(8),9,10,11,12,13
  tupletsRatios:array [3..13] of double = (2/3, 4/4, 4/5, 4/6, 4/7, 8/8, 8/9, 8/10, 8/11, 8/12, 8/13);

type
  TTimeMarker=record
    millisecond:integer;
    measureId:integer;
    trackId:integer;
    trackBeatId:integer;
    measureGridRatio:double;
    tempo:integer;
  end;
  TTimeMarkersArray=array of TTimeMarker;

  TMeasureTempoChunk=record
    startGridRatio:double;
    endGridRatio:double;
    tempo:integer;
    durationMS:double;
    hasTimeMarkerWithId:integer;
  end;

  TMeasureTimeInfo=record
    numerator:integer;
    denominator:integer;
    durationMS:double;
    tempoChunks:array of TMeasureTempoChunk;
    gridBeatsStartMs:array of double;
    tracksBeatsStartMs:array of array of double;
    tracksBeatsGridRatio:array of array of double;
    tracksBeatsTimeMarkersIds:array of array of integer; // points to indexes from tab.timeMarkers[] ("-1" if has no marker)
  end;
  TMeasuresTimeInfoArray=array of TMeasureTimeInfo;

  TTabulature=record
    gpTab:TGPTabStructure;
    timeMarkers:TTimeMarkersArray;
    measuresTimeInfo:TMeasuresTimeInfoArray;
  end;
  PTTabulature=^TTabulature;

implementation

end.

