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
    BCMaterialProgressBarMarquee1: TBCMaterialProgressBarMarquee;
    Image1: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
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
  X: Integer;
begin
  X:= Scale96ToScreen(64);
  Image1.Width:= X;
  Image1.Height:= X;
  Image1.ImageWidth:= X;
  Height:= X + 2*Image1.Top;
  Application.ProcessMessages;
end;

procedure TProgress.WriteLine1(const ACaption: String);
begin
  Label1.Caption:= ACaption;
  Application.ProcessMessages;
end;

procedure TProgress.WriteLine2(const ACaption: String);
begin
  Label2.Caption:= ACaption;
  Application.ProcessMessages;
end;

end.

