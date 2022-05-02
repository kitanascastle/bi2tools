#INCLUDE ONCE "dsound.inc"
#INCLUDE ONCE "mediaobj.inc"
#INCLUDE ONCE "mfapi.inc"
#INCLUDE ONCE "uuids.inc"

%LOCKFLAGS_NONE = 0
'
%PLAYFLAGS_NONE = 0
%PLAYFLAGS_LOOPING = 1
'
%COOPERATIVELEVEL_PRIORITY = 2
'
%DX_SOUNDEFFECT_NONE = 0
%DX_SOUNDEFFECT_FADEIN = 1
%DX_SOUNDEFFECT_FADEOUT = 2

$CLSID_WindowsMediaPlayer = "{CD3AFA76-B84F-48F0-9393-7EDC34128127}"



'Get IMediaObject from unknown interface
FUNCTION IUnknownToMediaObject(BYVAL pthis AS DWORD PTR, BYREF riid AS GUID, BYREF ppvObj AS IMediaObject) AS LONG
   LOCAL HRESULT AS LONG

   IF pthis = %NULL THEN FUNCTION = %E_POINTER : EXIT FUNCTION
   CALL DWORD @@pthis[0] USING IUnknownToMediaObject(pthis, riid, ppvObj) TO HRESULT

   FUNCTION = HRESULT
END FUNCTION



'Maps a Four-CC-Code to a GUID
FUNCTION DSFOURCCMap$(fourcc$)
  LOCAL g$

  g$ = "{"+HEX$(ASC(fourcc$, 4), 2)+HEX$(ASC(fourcc$, 3), 2)+HEX$(ASC(fourcc$, 2), 2)+HEX$(ASC(fourcc$, 1), 2)+"-0000-0010-8000-00AA00389B71}"

  DSFOURCCMap$ = g$
END FUNCTION



'Reads a file into memory
FUNCTION DSReadFileContent$(f$)
  LOCAL a$, fnr&, n&

  IF ISFILE(f$) = 0 THEN EXIT FUNCTION

  fnr& = FREEFILE
  OPEN f$ FOR BINARY LOCK SHARED AS fnr&
  n& = LOF(fnr&)
  GET$ fnr&, n&, a$
  CLOSE fnr&

  DSReadFileContent$ = a$
END FUNCTION



'Loads sound data from a WAV file
FUNCTION DSLoadWaveFile&(BYVAL f$, BYREF wavedata$, BYREF samplerate&, BYREF channels&)
  LOCAL wav$, chunkid$, fmt$, subchunkid$, audioformat&, bitspersample&, datachunkid$, datasize&

  wav$ = DSReadFileContent$(f$)
  IF wav$ = "" THEN EXIT FUNCTION

  'get header data
  chunkid$ = MID$(wav$, 1, 4)
  fmt$ = MID$(wav$, 9, 4)
  subchunkid$ = MID$(wav$, 13, 4)
  audioformat& = CVI(wav$, 21)
  channels& = CVI(wav$, 23)
  samplerate& = CVL(wav$, 25)
  bitspersample& = CVI(wav$, 35)
  datachunkid$ = MID$(wav$, 37, 4)
  datasize& = CVL(wav$, 41)
'PRINT "DSLoadWaveFile:"; audioformat&; channels&; samplerate&; bitspersample&

  'validate header
  IF chunkid$ <> "RIFF" OR fmt$ <> "WAVE" THEN
    DSLoadWaveFile& = -2
    EXIT FUNCTION
  END IF
  IF audioformat& <> 1 THEN
    DSLoadWaveFile& = -3
    EXIT FUNCTION
  END IF

  wavedata$ = MID$(wav$, 45, datasize&)
  DSLoadWaveFile& = 1
END FUNCTION



'Loads sound data from a VOC file
FUNCTION DSLoadVocFile&(BYVAL f$, BYREF wavedata$, BYREF samplerate&, BYREF channels&)
  LOCAL voc$, wav$, vocid$, ver&, tp&, length&, sr&, codec&, i&, sample&

  'read VOC data from file if not passed in "wavedata$"
  IF f$ <> "" AND wavedata$ = "" THEN
    voc$ = DSReadFileContent$(f$)
    IF voc$ = "" THEN EXIT FUNCTION
  ELSE
    voc$ = wavedata$
  END IF

  'get header data
  vocid$ = MID$(voc$, 1, 19)
  ver& = CVI(voc$, 23)
  tp& = ASC(voc$, 27)
  length& = CVL(MID$(voc$, 28, 3)+CHR$(0))

  'validate header
  IF vocid$ <> "Creative Voice File" THEN
    DSLoadVocFile& = -2
    EXIT FUNCTION
  END IF

  SELECT CASE tp&
  CASE 1:  'audio data
    sr& = ASC(voc$, 31)
    codec& = ASC(voc$, 32)
    samplerate& = 1000000/(256-sr&)
    channels& = 1
    IF codec& = 0 THEN
      'convert audio data from 8-bit to 16-bit
      wavedata$ = STRING$(length&*2, 0)
      FOR i& = 1 TO length&
        sample& = ASC(voc$, i&+32)
        sample& = (sample&-128)*256
        MID$(wavedata$, i&*2-1, 2) = MKI$(sample&)
      NEXT i&
    ELSE
      DSLoadVocFile& = -3
      EXIT FUNCTION
    END IF
  CASE ELSE:
    DSLoadVocFile& = -2
    EXIT FUNCTION
  END SELECT

  DSLoadVocFile& = 1
END FUNCTION



'==================================================================================================
'DirectSound class
'==================================================================================================
CLASS CDIRECTSOUND

INSTANCE i_directsound AS IDirectSound
INSTANCE i_soundchannelcount AS LONG                'number of sound channel objects
INSTANCE i_soundchannels() AS ICDIRECTSOUNDCHANNEL  'sound channels objects (sound that will be played in parallel)
INSTANCE i_wavecount AS LONG                        'number of cached wave data
INSTANCE i_wavedata() AS STRING                     'cached wave data
INSTANCE i_samplerates() AS LONG                    'sample rates of the wave data
INSTANCE i_channels() AS LONG                       'number of channels of the wave data (1 = mono , 2 = stereo)
INSTANCE i_mediaobject AS IMediaObject



'Constructor
CLASS METHOD CREATE
END METHOD



'Loads sound data using a decoder
CLASS METHOD DSLoadFileWithDecoder(BYVAL f$, BYREF wavedata$, BYREF samplerate&, BYREF channels&) AS LONG
  LOCAL r&
  LOCAL fourcc$
  LOCAL dmo_input_type AS DMO_MEDIA_TYPE, dmo_output_type AS DMO_MEDIA_TYPE
  LOCAL wf AS WAVEFORMATEX

  'create decoder object
  r& = ME.DSCreateMediaObject
  IF r& = 0 THEN
    METHOD = -4
    EXIT METHOD
  END IF

  'set input wave format
  fourcc$ = CHR$(&H55, 0, 0, 0)  'WAVE_FORMAT_MPEGLAYER3
  wf.wFormatTag = &H55
  wf.nSamplesPerSec = 44100
  wf.nChannels = 2
  wf.wBitsPerSample = 32
  wf.nBlockAlign = wf.nChannels*wf.wBitsPerSample/8
  wf.nAvgBytesPerSec = wf.nSamplesPerSec*wf.nChannels*wf.wBitsPerSample/8
  wf.cbSize = 0

  'set input type
  dmo_input_type.majortype = $MFMediaType_Audio
  dmo_input_type.subtype = GUID$(DSFOURCCMap$(fourcc$))
  dmo_input_type.formattype = $FORMAT_WaveFormatEx
  dmo_input_type.cbFormat = SIZEOF(WAVEFORMATEX)
  dmo_input_type.pbFormat = VARPTR(wf)
  dmo_input_type.bFixedSizeSamples = 1
  dmo_input_type.bTemporalCompression = 0
  dmo_input_type.lSampleSize = wf.nBlockAlign
  r& = i_mediaobject.SetInputType(0, dmo_input_type, 0)
  IF r& <> 0 THEN
PRINT "SetInputType error:", r&
    METHOD = -3
    EXIT METHOD
  END IF

  'set output wave format
  wf.wFormatTag = %WAVE_FORMAT_PCM
  wf.nSamplesPerSec = 44100
  wf.nChannels = 2
  wf.wBitsPerSample = 32
  wf.nBlockAlign = wf.nChannels*wf.wBitsPerSample/8
  wf.nAvgBytesPerSec = wf.nSamplesPerSec*wf.nBlockAlign
  wf.cbSize = 0

  'set output type
  dmo_output_type.majortype = $MFMediaType_Audio
  dmo_output_type.subtype = $MEDIASUBTYPE_PCM
  dmo_output_type.formattype = $FORMAT_WaveFormatEx
  dmo_output_type.cbFormat = SIZEOF(WAVEFORMATEX)
  dmo_output_type.pbFormat = VARPTR(wf)
  dmo_output_type.bFixedSizeSamples = 1
  dmo_output_type.bTemporalCompression = 0
  dmo_output_type.lSampleSize = wf.nBlockAlign
  r& = i_mediaobject.SetOutputType(0, dmo_output_type, 0)
  IF r& <> 0 THEN
PRINT "SetOutputType error:", r&
'WAITKEY$
    METHOD = -30
    EXIT METHOD
  END IF

END METHOD



'Creates the IMediaObject instance
CLASS METHOD DSCreateMediaObject AS LONG
  LOCAL hr&
  LOCAL pUnknown AS DWORD

  IF ISOBJECT(i_mediaobject) THEN
    METHOD = -1
    EXIT METHOD
  END IF

  'create an instance of the object
  hr& = CoCreateInstance(GUID$($CLSID_WindowsMediaPlayer), NOTHING, %CLSCTX_SERVER, $IID_IUnknown, pUnknown)
  IF hr& = %S_OK AND pUnknown <> %NULL THEN
      'convert the IUNKOWN interface to the read IMediaObject interface
      hr& = IUnknownToMediaObject(pUnknown, $IID_IDispatch, i_mediaobject)
   END IF

  IF ISOBJECT(i_mediaobject) THEN METHOD = 1
END METHOD



INTERFACE ICDIRECTSOUND : INHERIT IUNKNOWN

PROPERTY GET DirectSoundInterface AS IDirectSound
  PROPERTY = i_directsound
END PROPERTY

PROPERTY GET WaveCount AS LONG
  PROPERTY = i_wavecount
END PROPERTY

PROPERTY GET WaveData(index&) AS STRING
  LOCAL v$

  IF index& >= 0 AND index& <= UBOUND(i_wavedata) THEN v$ = i_wavedata(index&)
  PROPERTY = v$
END PROPERTY

PROPERTY GET SampleRate(index&) AS LONG
  LOCAL sr&

  IF index& >= 0 AND index& <= UBOUND(i_samplerates) THEN sr& = i_samplerates(index&)
  PROPERTY = sr&
END PROPERTY

PROPERTY GET Channels(index&) AS LONG
  LOCAL ch&

  IF index& >= 0 AND index& <= UBOUND(i_channels) THEN ch& = i_channels(index&)
  PROPERTY = ch&
END PROPERTY



'Initializes DirectSound
METHOD InitDirectSound(BYVAL hwnd&, BYVAL maxsoundchannels&) AS LONG
  LOCAL r&

  r& = DirectSoundCreate(BYVAL %NULL, i_directsound)

  'set Cooperative Level to PRIORITY (priority level can call the SetFormat and Compact methods)
  i_directsound.SetCooperativeLevel(hwnd&, %COOPERATIVELEVEL_PRIORITY)

  i_soundchannelcount = 0
  DIM i_soundchannels(maxsoundchannels&-1)

  i_wavecount = 0
  DIM i_wavedata(31), i_samplerates(31), i_channels(31)

  METHOD = %TRUE
END METHOD



'Loads a sound file
'Supported file formats: WAV + VOC
'Return codes: 1 = Success
'              0 = file not found
'             -1 = unsupported file type
'             -2 = bad file header
'             -3 = unsupported coded
'             -4 = error loading decoder
METHOD LoadSoundFile(BYVAL f$, BYREF wavedata$, BYREF samplerate&, BYREF channels&) AS LONG
  LOCAL e$, r&

  e$ = UCASE$(PATHNAME$(EXTN, f$))
  SELECT CASE e$
  CASE ".WAV"
    r& = DSLoadWaveFile&(f$, wavedata$, samplerate&, channels&)
  CASE ".VOC"
    r& = DSLoadVocFile&(f$, wavedata$, samplerate&, channels&)
  CASE ELSE
    r& = ME.DSLoadFileWithDecoder(f$, wavedata$, samplerate&, channels&)
  END SELECT

'IF r& <> 0 THEN PRINT "LoadSoundFile error: "+FORMAT$(r&)

  METHOD = r&
END METHOD



'Store wave data
'The wave data must be 16 bit mono
METHOD AddWaveData(w AS STRING, sr AS LONG, ch AS LONG) AS LONG
  IF i_wavecount > UBOUND(i_wavedata) THEN REDIM PRESERVE i_wavedata(i_wavecount+31), i_samplerates(i_wavecount+31), i_channels(i_wavecount+31)
  i_wavedata(i_wavecount) = w
  i_samplerates(i_wavecount) = sr
  i_channels(i_wavecount) = ch
  i_wavecount = i_wavecount+1

  METHOD = i_wavecount-1
END METHOD



'Create a new sound channel
METHOD CreateSoundChannel(soundnumber&) AS ICDIRECTSOUNDCHANNEL
  IF i_soundchannelcount > UBOUND(i_soundchannels) THEN EXIT METHOD

  'create new sound channel
  i_soundchannels(i_soundchannelcount) = CLASS "CDIRECTSOUNDCHANNEL"
  i_soundchannels(i_soundchannelcount).InitChannel(ME, soundnumber&)
  i_soundchannelcount = i_soundchannelcount+1

  METHOD = i_soundchannels(i_soundchannelcount-1)
END METHOD



'Plays all channels
METHOD PlayAllChannels(millisecsToPlay&)
  LOCAL i&

  FOR i& = 0 TO i_soundchannelcount-1
    IF i_soundchannels(i&).IsPlaying = 1 THEN i_soundchannels(i&).FillBuffer(millisecsToPlay&)
  NEXT i&
END METHOD

END INTERFACE

END CLASS



'==================================================================================================
'DirectSoundChannel class
'==================================================================================================
CLASS CDIRECTSOUNDCHANNEL

INSTANCE i_ds AS ICDIRECTSOUND
INSTANCE i_isplaying AS LONG             'set to 0 if this channel is stopped
INSTANCE i_bytespersecond AS LONG        'number of bytes to play per second
INSTANCE i_soundnumber AS LONG           'sound to play (index in i_wavedata)
INSTANCE i_sourceposition AS LONG        'current position in the wave data
INSTANCE i_buffer AS IDirectSoundBuffer  'the sound buffer
INSTANCE i_buffersize AS LONG            'size of the sound buffer
INSTANCE i_bufferpos AS LONG             'write position into the sound buffer
INSTANCE i_elapsedtime AS LONG           'total number of milliseconds played so far
INSTANCE i_volumeleft AS SINGLE          'volume of the left speaker
INSTANCE i_volumeright AS SINGLE         'volume of the right speaker
INSTANCE i_soundeffect AS LONG           'sound effect of the channel
INSTANCE i_soundeffectparam AS SINGLE    'parameter of the sound effect
INSTANCE i_soundeffectstarttime AS LONG  'time (in milliseconds) when the sound effect was activated
INSTANCE i_bufferreadpos AS LONG         'current read position in the sound buffer
INSTANCE i_bufferwritepos AS LONG        'current write position in the sound buffer


INTERFACE ICDIRECTSOUNDCHANNEL : INHERIT IUNKNOWN

PROPERTY GET IsPlaying AS LONG
  PROPERTY = i_isplaying
END PROPERTY
PROPERTY SET IsPlaying(BYVAL v AS LONG)
  i_isplaying = IIF&(v = 0, 0, 1)
END PROPERTY
'
PROPERTY GET SoundNumber AS LONG
  PROPERTY = i_soundnumber
END PROPERTY
'
PROPERTY GET BufferSize AS LONG
  PROPERTY = i_buffersize
END PROPERTY
'
PROPERTY GET BufferReadPosition AS LONG
  PROPERTY = i_bufferreadpos
END PROPERTY
'
PROPERTY GET BufferWritePosition AS LONG
  PROPERTY = i_bufferwritepos
END PROPERTY
'
PROPERTY GET BufferSourcePosition AS LONG
  PROPERTY = i_sourceposition
END PROPERTY


'Initializes the sound channel
METHOD InitChannel(ds AS ICDIRECTSOUND, soundnumber&)
  LOCAL maxsize&
  LOCAL bufferdescr AS DSBUFFERDESC
  LOCAL wfx AS WAVEFORMATEX
  LOCAL dx AS IDirectSound

  IF soundnumber& < 0 OR soundnumber& >= ds.WaveCount THEN EXIT METHOD

  'set format
  wfx.wFormatTag = %WAVE_FORMAT_PCM
  wfx.nChannels = 2
  wfx.wBitsPerSample = 16
  wfx.nSamplesPerSec = ds.SampleRate(soundnumber&)
  wfx.nBlockAlign = wfx.wBitsPerSample*wfx.nChannels/8
  wfx.nAvgBytesPerSec = wfx.nSamplesPerSec*wfx.nBlockAlign

  'set buffer properties
  maxsize& = LEN(ds.WaveData(soundnumber&))*2/ds.Channels(soundnumber&)
  i_buffersize = MIN&(maxsize&, wfx.nSamplesPerSec*4*2)  '2 channels (stereo), 16 bit per sample, 2 seconds length
  bufferdescr.dwSize = SIZEOF(DSBUFFERDESC)
  bufferdescr.dwFlags = %DSBCAPS_GETCURRENTPOSITION2 OR %DSBCAPS_CTRLPOSITIONNOTIFY OR %DSBCAPS_GLOBALFOCUS OR %DSBCAPS_CTRLVOLUME OR %DSBCAPS_STICKYFOCUS
  bufferdescr.dwBufferBytes = i_buffersize
  bufferdescr.lpwfxFormat = VARPTR(wfx)
  bufferdescr.guid3DAlgorithm = $DS3DALG_DEFAULT

  'create sound buffer
  dx = ds.DirectSoundInterface
  dx.CreateSoundBuffer(bufferdescr, i_buffer)

  'set channel properties
  i_ds = ds
  i_isplaying = 0
  i_bytespersecond = wfx.nSamplesPerSec*4
  i_soundnumber = soundnumber&
  i_sourceposition = 0
  i_bufferpos = 0
  i_volumeleft = 0
  i_volumeright = 0
END METHOD



'Sets the volume of the sound channel
METHOD SetVolume(volumeLeft!, volumeRight!)
  i_volumeleft = MIN(1.0, MAX(0.0, volumeLeft!))
  i_volumeright = MIN(1.0, MAX(0.0, volumeRight!))
END METHOD



'Sets the sound effect for this channel
'effect&:
'  %DX_SOUNDEFFECT_FADEIN : Increase the volume from 0 to the volume set with SetVolume() over a timespan of effectparam! seconds
'  %DX_SOUNDEFFECT_FADEOUT : Decrease the volume from the volume set with SetVolume() to 0 over a timespan of effectparam! seconds
'effectparam! : number of seconds
METHOD SetSoundEffect(effect&, effectparam!)
  i_soundeffect = effect&
  i_soundeffectparam = MAX(0.05, effectparam!)*1000  'convert to milliseconds (and make sure at least 50 ms are applied because the algorithm divides by this value)
  i_soundeffectstarttime = i_elapsedtime
END METHOD



'Start playing
'flag = %PLAYFLAGS_NONE or %PLAYFLAGS_LOOPING
METHOD PLAY(flags AS LONG)
  i_isplaying = 1
  i_elapsedtime = 0
  i_soundeffect = %DX_SOUNDEFFECT_NONE
  i_buffer.Play(0, 0, flags)
END METHOD



'Stop playing
METHOD STOP
  i_isplaying = 0
  i_buffer.Stop
END METHOD



'Plays the next sequence by adding it to the buffer
'length : number of milliseconds to play
'THIS FUNCTION MUST BE CALLED AT LEAST ONCE PER SECOND OR THE SOUND WILL START STUTTERING!
METHOD FillBuffer(length&)
  LOCAL i&, n1&, n2&, samplecount&, sample&, srcdata$, pbuffer1&, pbuffer2&, nbuffer1&, nbuffer2&
  LOCAL volleft!, volright!, timestamp!
  LOCAL toplay%()

  'setting volumes to 0 stops the channel
  IF i_volumeleft <= 0 AND i_volumeright <= 0 THEN
    ME.Stop
    EXIT METHOD
  END IF

  'get wave data to play
  samplecount& = INT((i_bytespersecond*length&/1000+3)/4)
  srcdata$ = MID$(i_ds.WaveData(i_soundnumber), i_sourceposition+1, samplecount&*2)
  IF LEN(srcdata$) < samplecount&*2 THEN
    i_sourceposition = samplecount&*2-LEN(srcdata$)
    srcdata$ = srcdata$+LEFT$(i_ds.WaveData(i_soundnumber), i_sourceposition)
  ELSE
    i_sourceposition = i_sourceposition+samplecount&*2
  END IF

  'copy the next sequence from the wave input to the local buffer, convert it to stereo and apply volume
  DIM toplay%(samplecount&*2-1)
  FOR i& = 0 TO samplecount&-1
    'apply sound effect
    timestamp! = i_elapsedtime+i&*length&/samplecount&-i_soundeffectstarttime
    volleft! = i_volumeleft
    volright! = i_volumeright
    SELECT CASE i_soundeffect
    CASE %DX_SOUNDEFFECT_FADEIN
      volleft! = MAX(0, MIN(i_volumeleft, volleft!*timestamp!/i_soundeffectparam))
      volright! = MAX(0, MIN(i_volumeright, volright!*timestamp!/i_soundeffectparam))
    CASE %DX_SOUNDEFFECT_FADEOUT
      volleft! = MAX(0, MIN(i_volumeleft, i_volumeleft-volleft!*timestamp!/i_soundeffectparam))
      volright! = MAX(0, MIN(i_volumeright, i_volumeright-volright!*timestamp!/i_soundeffectparam))
    END SELECT
    'copy wave data
    sample& = CVI(srcdata$, i&*2+1)
    toplay%(i&*2) = INT(sample&*volleft!)     'write left channel
    toplay%(i&*2+1) = INT(sample&*volright!)  'write right channel
  NEXT i&
  i_elapsedtime = i_elapsedtime+length&

  'copy local buffer to DirectSound buffer
  i_buffer.Lock(i_bufferpos, samplecount&*4, pbuffer1&, nbuffer1&, pbuffer2&, nbuffer2&, %LOCKFLAGS_NONE)  'buffer is circular, so there will always be sampleCount * 4 bytes available no matter at which position

  n1& = MIN&(nbuffer1&, samplecount&*4)  'number of bytes to be written to the first buffer
  POKE$ pbuffer1&, PEEK$(VARPTR(toplay%(0)), n1&)
  IF n1& < samplecount&*4 AND pbuffer2& <> 0 THEN
    'the first buffer is at the end of the circular buffer structure and there is not enough space for the entire source data, so a second buffer (at the beginning of the circular structure) must be filled with the rest of the data
    n2& = MIN&(nbuffer2&, samplecount&*4-n1&)
    POKE$ pbuffer2&, PEEK$(VARPTR(toplay%(0))+n1&, n2&)
  END IF
  i_bufferpos = i_bufferpos+samplecount&*4
  IF i_bufferpos >= i_buffersize THEN i_bufferpos = i_bufferpos-i_buffersize
  i_buffer.Unlock(pbuffer1&, n1&, pbuffer2&, n2&)

  'get new read/write position
  i_buffer.GetCurrentPosition(i_bufferreadpos, i_bufferwritepos)
END METHOD

END INTERFACE

END CLASS
