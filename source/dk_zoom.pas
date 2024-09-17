unit DK_Zoom;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, ComCtrls, DK_StrUtils, DK_CtrlUtils;

type

  TZoomEvent = procedure(const AZoomPercent: Integer) of object;

  { TZoomForm }

  TZoomForm = class(TForm)
    ZoomImages16: TImageList;
    ZoomImages20: TImageList;
    ZoomImages24: TImageList;
    ZoomImages28: TImageList;
    ValueLabel: TLabel;
    TitleLabel: TLabel;
    ZoomInButton: TSpeedButton;
    ZoomTrackBar: TTrackBar;
    ZoomOutButton: TSpeedButton;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ZoomInButtonClick(Sender: TObject);
    procedure ZoomOutButtonClick(Sender: TObject);
    procedure ZoomTrackBarChange(Sender: TObject);
  private
    OnZoomChange: TZoomEvent;
    procedure SetValueLabel;
  public

  end;

var
  ZoomForm: TZoomForm;

  function CreateZoomControls(const AMinPercent, AMaxPercent, ACurrentPercent: Integer;
                          const APanel: TPanel;
                          const AOnZoomChange: TZoomEvent;
                          const AAlignRight: Boolean = False): TZoomForm;

implementation

function CreateZoomControls(const AMinPercent, AMaxPercent, ACurrentPercent: Integer;
                        const APanel: TPanel;
                        const AOnZoomChange: TZoomEvent;
                        const AAlignRight: Boolean = False): TZoomForm;
begin
  APanel.AutoSize:= False;
  Result:= TZoomForm.Create(APanel);
  Result.ZoomTrackBar.Min:= AMinPercent;
  Result.ZoomTrackBar.Max:= AMaxPercent;
  Result.ZoomTrackBar.Position:= ACurrentPercent;
  if AAlignRight then
    Result.Align:= alRight
  else
    Result.Align:= alLeft;
  Result.Color:= APanel.Color;
  Result.Parent:= APanel;
  Result.Parent.Caption:= EmptyStr;
  Result.OnZoomChange:= AOnZoomChange;
  Result.Show;
end;

{$R *.lfm}

{ TZoomForm }

procedure TZoomForm.FormShow(Sender: TObject);
var
  n: Integer;
begin
  ZoomOutButton.Images:= ChooseImageListForScreenPPI(ZoomImages16, ZoomImages20,
                                                     ZoomImages24, ZoomImages28);
  ZoomInButton.Images:= ZoomOutButton.Images;

  ValueLabel.Width:= SWidth('0000 %', ValueLabel.Font);
  SetValueLabel;
  {$IFDEF WINDOWS}
  ZoomTrackBar.BorderSpacing.Top:= Scale96ToScreen(2);
  {$ENDIF}
  Constraints.MinHeight:= Scale96ToScreen(18);
  n:= ZoomTrackBar.Height + Scale96ToScreen(4);
  if Constraints.MinHeight<n then
    Constraints.MinHeight:= n;
  Constraints.MaxHeight:= Constraints.MinHeight;
  AutoSize:= True;
end;

procedure TZoomForm.FormChangeBounds(Sender: TObject);
begin
  Parent.Height:= Height;
end;

procedure TZoomForm.ZoomInButtonClick(Sender: TObject);
begin
  ZoomTrackBar.Position:= ZoomTrackBar.Position + 5;
end;

procedure TZoomForm.ZoomOutButtonClick(Sender: TObject);
begin
  ZoomTrackBar.Position:= ZoomTrackBar.Position - 5;
end;

procedure TZoomForm.ZoomTrackBarChange(Sender: TObject);
begin
  SetValueLabel;
  if Assigned(OnZoomChange) then
    OnZoomChange(ZoomTrackBar.Position);
end;

procedure TZoomForm.SetValueLabel;
begin
  ValueLabel.Caption:= IntToStr(ZoomTrackBar.Position) + ' %';
end;

end.

