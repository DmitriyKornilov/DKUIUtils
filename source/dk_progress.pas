unit DK_Progress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls,
  BCMaterialProgressBarMarquee;

type

  { TProgress }

  TProgress = class(TForm)
    ProgressBar: TBCMaterialProgressBarMarquee;
    ProgressImage: TImage;
    ProgressImages64: TImageList;
    ProgressImages80: TImageList;
    ProgressImages96: TImageList;
    ProgressImages112: TImageList;
    ProgressLabel1: TLabel;
    ProgressLabel2: TLabel;
    ProgressPanel: TPanel;
    procedure FormShow(Sender: TObject);
  private

  public
    procedure WriteLine1(const ACaption: String);
    procedure WriteLine2(const ACaption: String);
  end;

var
  Progress: TProgress;

implementation

{$R *.lfm}

procedure TProgress.FormShow(Sender: TObject);
var
  X, Y: Integer;
begin
  X:= Scale96ToScreen(64);
  ProgressImage.Width:= X;
  ProgressImage.Height:= X;
  Y:= Scale96ToScreen(6);
  Height:= X + 2*Y + 2;
  ProgressImage.Left:= Y;
  ProgressImage.Top:= Y;

  case Screen.PixelsPerInch of
    96 : ProgressImage.Images:= ProgressImages64;
    120: ProgressImage.Images:= ProgressImages80;
    144: ProgressImage.Images:= ProgressImages96;
    168: ProgressImage.Images:= ProgressImages112;
  end;


  Application.ProcessMessages;
end;

procedure TProgress.WriteLine1(const ACaption: String);
begin
  ProgressLabel1.Caption:= ACaption;
  Application.ProcessMessages;
end;

procedure TProgress.WriteLine2(const ACaption: String);
begin
  ProgressLabel2.Caption:= ACaption;
  Application.ProcessMessages;
end;

end.

