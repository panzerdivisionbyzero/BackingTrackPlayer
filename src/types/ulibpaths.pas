{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit ULibPaths;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  {$IFDEF Windows}
    {$if defined(cpu64)}
      PA_FileName = 'lib\Windows\64bit\LibPortaudio-64.dll';
      SF_FileName = 'lib\Windows\64bit\LibSndFile-64.dll';
      MP_FileName = 'lib\Windows\64bit\LibMpg123-64.dll';
	    ST_FileName = 'lib\Windows\64bit\plugin\LibSoundTouch-64.dll';
    {$else}
      PA_FileName = 'lib\Windows\32bit\LibPortaudio-32.dll';
      SF_FileName = 'lib\Windows\32bit\LibSndFile-32.dll';
      MP_FileName = 'lib\Windows\32bit\LibMpg123-32.dll';
	    ST_FileName = 'lib\Windows\32bit\plugin\LibSoundTouch-32.dll';
    {$endif}
  {$ENDIF}

  {$if defined(cpu64) and defined(linux) }
    SF_FileName = 'lib/Linux/64bit/LibSndFile-64.so';
    PA_FileName = 'lib/Linux/64bit/LibPortaudio-64.so';
    MP_FileName = 'lib/Linux/64bit/LibMpg123-64.so';
    ST_FileName = 'lib/Linux/64bit/plugin/LibSoundTouch-64.so';
  {$ENDIF}

  {$if defined(cpu86) and defined(linux)}
    PA_FileName = 'lib/Linux/32bit/LibPortaudio-32.so';
    SF_FileName = 'lib/Linux/32bit/LibSndFile-32.so';
    MP_FileName = 'lib/Linux/32bit/LibMpg123-32.so';
	ST_FileName = 'lib/Linux/32bit/plugin/LibSoundTouch-32.so';
  {$ENDIF}

  {$if defined(linux) and defined(cpuarm)}
    PA_FileName = 'lib/Linux/arm_raspberrypi/libportaudio-arm.so';
    SF_FileName = 'lib/Linux/arm_raspberrypi/libsndfile-arm.so';
    MP_FileName = 'lib/Linux/arm_raspberrypi/libmpg123-arm.so';
	  ST_FileName = 'lib/Linux/arm_raspberrypi/plugin/libsoundtouch-arm.so';
  {$ENDIF}

  {$IFDEF freebsd}
    {$if defined(cpu64)}
      PA_FileName = 'lib/FreeBSD/64bit/libportaudio-64.so';
      SF_FileName = 'lib/FreeBSD/64bit/libsndfile-64.so';
      MP_FileName = 'lib/FreeBSD/64bit/libmpg123-64.so';
    	ST_FileName = 'lib/FreeBSD/64bit/plugin/libsoundtouch-64.so';
    {$else}
      PA_FileName = 'lib/FreeBSD/32bit/libportaudio-32.so';
      SF_FileName = 'lib/FreeBSD/32bit/libsndfile-32.so';
      MP_FileName = 'lib/FreeBSD/32bit/libmpg123-32.so';
    	ST_FileName = 'lib/FreeBSD/32bit/libmpg123-32.so';
    {$endif}
  {$ENDIF}

  {$IFDEF Darwin}
    PA_FileName = '/lib/Mac/32bit/LibPortaudio-32.dylib';
    SF_FileName = '/lib/Mac/32bit/LibSndFile-32.dylib';
    MP_FileName = '/lib/Mac/32bit/LibMpg123-32.dylib';
	  ST_FileName = '/lib/Mac/32bit/plugin/LibSoundTouch-32.dylib';
  {$ENDIF}

implementation

end.


