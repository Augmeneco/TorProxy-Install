unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  utils, zipper, windows, Process;

type

  { TMainWin }

  TMainWin = class(TForm)
    Button1: TButton;
    LoggerMemo: TMemo;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  MainWin: TMainWin;
  requests: TRequests;

implementation

{$R *.lfm}

{ TMainWin }

procedure TMainWin.Button1Click(Sender: TObject);
var
  PSecAtr: LPSECURITY_ATTRIBUTES;
  UnZip: TUnZipper;
  torrc,exeoutput: String;
begin
  PSecAtr := Nil;
  UnZip := TUnZipper.Create;

  logWrite('Download Tor Bundle');
  if not FileExists('tor-win.zip') then
    requests.Download(
        'https://dist.torproject.org/torbrowser/9.5/tor-win32-0.4.3.5.zip',
        'tor-win.zip'
    );

  logWrite('Unpack tor-win.zip');
  CreateDirectory('C:\tor',PSecAtr);
  if not FileExists('tor-win.zip') then
  begin
    logWrite('Something went wrong');
    logWrite('Stopping');
    Exit;
  end;
  UnZip.OutputPath := 'C:\tor';
  UnZip.UnZipAllFiles('tor-win.zip');

  logWrite('Create Tor''s config');
  torrc := Format(
       'DataDirectory %s\Data\Tor'+#13+
       'GeoIPFile %s\Data\Tor\geoip'+#13+
       'GeoIPv6File  %s\Data\Tor\geoip6'+#13+
       'AvoidDiskWrites 1'+#13+
       'SocksPort 127.0.0.1:9050',
       ['C:\tor','C:\tor','C:\tor']
  );
  utils.WriteFile('C:\tor\Data\Tor\torrc',torrc);

  logWrite('Create rule in firewall');
  RunCommand(
    'netsh',
    [
      'advfirewall',
      'firewall',
      'add',
      'rule',
      'name="Tor Proxy"',
      'dir=in',
      'action=allow',
      'localport=9050',
      'protocol=tcp'
    ],exeoutput
  );

  logWrite('Create Tor''s service');
  RunCommand(
    'C:\tor\Tor\tor.exe',
    [
      '--service',
      'install'
    ],exeoutput
  );

  ShellExecute(MainWin.Handle, 'runas', 'net','start tor',nil, SW_SHOWNORMAL);
  sleep(5000);

  logWrite('Checking Tor''s working...');
  requests.Get('http://127.0.0.1:9050');
  if requests.error = 'Couldn''t connect to server' then
    logWrite('Something go wrong :/')
  else
    logWrite('Tor successfully installed');
end;

begin
  requests := TRequests.Create;
end.

