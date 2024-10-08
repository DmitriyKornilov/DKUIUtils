unit DK_Progress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls,
  BCMaterialProgressBarMarquee,
  DK_StrUtils, DK_CtrlUtils;

const
  CAPTION_END_SYMBOL = '...';

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
    procedure Go;
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
  ProgressImage.Images:= ChooseImageListForScreenPPI(ProgressImages64, ProgressImages80,
                                                     ProgressImages96, ProgressImages112);
  Go;
end;

procedure TProgress.WriteLine1(const ACaption: String);
begin
  ProgressLabel1.Caption:= SFit(ACaption, CAPTION_END_SYMBOL,
                                ProgressLabel1.Width, ProgressLabel1.Font);
  Go;
end;

procedure TProgress.WriteLine2(const ACaption: String);
begin
  ProgressLabel2.Caption:= SFit(ACaption, CAPTION_END_SYMBOL,
                                ProgressLabel2.Width, ProgressLabel2.Font);
  Go;
end;

procedure TProgress.Go;
begin
  Application.ProcessMessages;
end;

end.

