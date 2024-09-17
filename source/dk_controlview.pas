unit DK_ControlView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls, Forms, DividerBevel,
  BCPanel, BCButton, BCTypes, ColorSpeedButton,
  DK_Color, DK_CtrlUtils, DK_StrUtils;


type
  TCategoryButtons = array of TBCButton;
  TToolButtons = array of TColorSpeedButton;
  TBCPanels =  array of TBCPanel;
  TDividerBevels = array of TDividerBevel;

  { TControlView }

  TControlView = class (TObject)
  private
    FGlyphPX24: TImageList;
    FGlyphPX42: TImageList;
    FGlyphPX36: TImageList;
    FGlyphPX30: TImageList;

    FToolPanels: TBCPanels;
    FCaptionPanels: TBCPanels;
    FBorderPanels: TBCPanels;

    FDividerBevels: TDividerBevels;

    FCategoryButtons: TCategoryButtons;
    FToolButtons: TToolButtons;
    FDialogButtons: TToolButtons;

    function GetMainBGColor: TColor;
    function GetExtraBGColor: TColor;
    function GetBorderColor: TColor;

    procedure SetPanelBorder(const APanel: TBCPanel);
    procedure SetBorderSpacing(const AControl: TControl;
                               const AAround, ATop, ABottom, ALeft, ARight: Integer);

    procedure SetToolPanels;
    procedure SetCaptionPanels;
    procedure SetBorderPanels;

    procedure SetDividerBevels;

    procedure SetCategoryButtons;
    procedure SetToolButtons;
    procedure SetDialogButtons;
  public
    constructor Create(const AGlyphPX24, AGlyphPX30, AGlyphPX36, AGlyphPX42: TImageList);
    function Images: TImageList;

    procedure ToolPanels(const AControls: TBCPanels);
    procedure CaptionPanels(const AControls: TBCPanels);
    procedure BorderPanels(const AControls: TBCPanels);

    procedure DividerBevels(const AControls: TDividerBevels);

    procedure CategoryButtons(const AControls: TCategoryButtons);
    procedure ToolButtons(const AControls: TToolButtons);
    procedure DialogButtons(const AControls: TToolButtons);

    procedure ViewApply;
  end;

implementation

const
  //sizes for 96 PPI
  TOOL_PANEL_HEIGHT_DEFAULT  = 32;
  TOOL_BUTTON_WIDTH_DEFAULT  = TOOL_PANEL_HEIGHT_DEFAULT;
  EDIT_BUTTON_HEIGHT_DEFAULT = TOOL_PANEL_HEIGHT_DEFAULT - 2;
  EDIT_BUTTON_WIDTH_EXTRA    = 6;

  BORDER_SPACING_DEFAULT = 2;

{ TControlView }

function TControlView.Images: TImageList;
var
  PPI: Integer;
begin
  PPI:= Screen.PixelsPerInch;
  if PPI<108 then
    Result:= FGlyphPX24
  else if PPI<132 then
    Result:= FGlyphPX30
  else if PPI<156 then
    Result:= FGlyphPX36
  else
    Result:= FGlyphPX42;

  //case Screen.PixelsPerInch of
  //  96 : Result:= FGlyphPX24;
  //  120: Result:= FGlyphPX30;
  //  144: Result:= FGlyphPX36;
  //  168: Result:= FGlyphPX42;
  //end;
end;

function TControlView.GetMainBGColor: TColor;
begin
  Result:= clBtnFace;
end;

function TControlView.GetExtraBGColor: TColor;
begin
  Result:= ColorIncLightness(GetMainBGColor, -12);
end;

function TControlView.GetBorderColor: TColor;
begin
  Result:= ColorIncLightness(GetMainBGColor, -70);
end;

procedure TControlView.SetPanelBorder(const APanel: TBCPanel);
begin
  APanel.BorderBCStyle:= bpsBorder;
  APanel.Border.Color:= GetBorderColor;
  APanel.Border.Style:= bboSolid;
  APanel.Rounding.RoundX:= 0;
  APanel.Rounding.RoundY:= 0;
  APanel.BevelInner:= bvNone;
  APanel.BevelOuter:= bvNone;
  APanel.BevelWidth:= 1;
end;

procedure TControlView.SetBorderSpacing(const AControl: TControl;
  const AAround, ATop, ABottom, ALeft, ARight: Integer);
begin
  AControl.BorderSpacing.Around:= AAround;
  AControl.BorderSpacing.Top:= ATop;
  AControl.BorderSpacing.Bottom:= ABottom;
  AControl.BorderSpacing.Left:= ALeft;
  AControl.BorderSpacing.Right:= ARight;
end;

procedure TControlView.SetToolPanels;
var
  i, h: Integer;
begin
  if Length(FToolPanels)=0 then Exit;

  h:= TOOL_PANEL_HEIGHT_DEFAULT;
  for i:= 0 to High(FToolPanels) do
  begin
    FToolPanels[i].AutoSize:= False;
    ControlHeight(FToolPanels[i], h);
    FToolPanels[i].Caption:= EmptyStr;
    FToolPanels[i].Background.Style:= bbsColor;
    FToolPanels[i].Background.Color:= GetMainBGColor;
    SetPanelBorder(FToolPanels[i]);
  end;
end;

procedure TControlView.SetCaptionPanels;
var
  i, h: Integer;
begin
  if Length(FCaptionPanels)=0 then Exit;

  h:= Round(TOOL_PANEL_HEIGHT_DEFAULT*0.7);
  for i:= 0 to High(FCaptionPanels) do
  begin
    ControlHeight(FCaptionPanels[i], h);
    FCaptionPanels[i].Background.Color:= GetExtraBGColor;
    SetPanelBorder(FCaptionPanels[i]);
  end;
end;

procedure TControlView.SetBorderPanels;
var
  i: Integer;
begin
  if Length(FBorderPanels)=0 then Exit;

  for i:= 0 to High(FBorderPanels) do
    SetPanelBorder(FBorderPanels[i]);
end;

procedure TControlView.SetDividerBevels;
var
  i: Integer;
begin
  if Length(FDividerBevels)=0 then Exit;

  for i:= 0 to High(FDividerBevels) do
  begin
    SetBorderSpacing(FDividerBevels[i], BORDER_SPACING_DEFAULT, 0, 0, 0, 0);
    FDividerBevels[i].Caption:= EmptyStr;
    FDividerBevels[i].Color:= GetMainBGColor;
    FDividerBevels[i].BevelWidth:= 1;
    {$IFDEF WINDOWS}
    FDividerBevels[i].BevelStyle:= bsLowered;
    {$ENDIF}
    {$IFDEF LINUX}
    FDividerBevels[i].BevelStyle:= bsRaised;
    {$ENDIF}
  end;
end;

procedure TControlView.SetCategoryButtons;
var
  i: Integer;
  ImageList: TImageList;
begin
  if Length(FCategoryButtons)=0 then Exit;

  ImageList:= Images;
  for i:= 0 to High(FCategoryButtons) do
  begin
    SetBorderSpacing(FCategoryButtons[i], BORDER_SPACING_DEFAULT, 0, 0, 0, 0);
    FCategoryButtons[i].Images:= ImageList;
    FCategoryButtons[i].StateNormal.Background.Color:= GetExtraBGColor;
    FCategoryButtons[i].StateNormal.Border.Color:= GetBorderColor;
    FCategoryButtons[i].Cursor:= crHandPoint;
  end;
end;

procedure TControlView.SetToolButtons;
var
  i: Integer;
  ImageList: TImageList;
begin
  if Length(FToolButtons)=0 then Exit;

  ImageList:= Images;
  for i:= 0 to High(FToolButtons) do
  begin
    FToolButtons[i].AutoSize:= False;
    SetBorderSpacing(FToolButtons[i], BORDER_SPACING_DEFAULT, 0, 0, 0, 0);

    ControlWidth(FToolButtons[i], TOOL_BUTTON_WIDTH_DEFAULT);
    FToolButtons[i].Images:= ImageList;
    FToolButtons[i].StateNormal.Color:= GetExtraBGColor;
    FToolButtons[i].StateNormal.BorderColor:= GetBorderColor;
    FToolButtons[i].StateNormal.BorderWidth:= 1;

    FToolButtons[i].Cursor:= crHandPoint;
  end;
end;

procedure TControlView.SetDialogButtons;
var
  i, W, H: Integer;
  C: TControl;
  ImageList: TImageList;
begin
  if Length(FDialogButtons)=0 then Exit;

  C:= FDialogButtons[0].Parent;
  H:= C.Scale96ToForm(EDIT_BUTTON_HEIGHT_DEFAULT);
  W:= FDialogButtons[0].Width;
  for i:= 1 to High(FDialogButtons) do
    if SLength(FDialogButtons[i].Caption)>SLength(FDialogButtons[i-1].Caption) then
      W:= FDialogButtons[i].Width;
  W:= W + C.Scale96ToForm(EDIT_BUTTON_WIDTH_EXTRA);
  for i:= 0 to High(FDialogButtons) do
  begin
    FDialogButtons[i].Constraints.MinHeight:= H;
    FDialogButtons[i].Constraints.MinWidth:= W;
    FDialogButtons[i].AutoSize:= True;
  end;

  ImageList:= Images;
  for i:= 0 to High(FDialogButtons) do
  begin
    //SetBorderSpacing(FDialogButtons[i], BORDER_SPACING_DEFAULT, 0, 0, 0, 0);

    FDialogButtons[i].Images:= ImageList;
    FDialogButtons[i].StateNormal.Color:= GetExtraBGColor;
    FDialogButtons[i].StateNormal.BorderColor:= GetBorderColor;
    FDialogButtons[i].StateNormal.BorderWidth:= 1;

    FDialogButtons[i].Cursor:= crHandPoint;
  end;
end;

constructor TControlView.Create(const AGlyphPX24, AGlyphPX30, AGlyphPX36,
  AGlyphPX42: TImageList);
begin
  FGlyphPX24:= AGlyphPX24;
  FGlyphPX30:= AGlyphPX30;
  FGlyphPX36:= AGlyphPX36;
  FGlyphPX42:= AGlyphPX42;
end;

procedure TControlView.ToolPanels(const AControls: TBCPanels);
begin
  FToolPanels:= AControls;
end;

procedure TControlView.CaptionPanels(const AControls: TBCPanels);
begin
  FCaptionPanels:= AControls;
end;

procedure TControlView.BorderPanels(const AControls: TBCPanels);
begin
  FBorderPanels:= AControls;
end;

procedure TControlView.DividerBevels(const AControls: TDividerBevels);
begin
  FDividerBevels:= AControls;
end;

procedure TControlView.CategoryButtons(const AControls: TCategoryButtons);
begin
  FCategoryButtons:= AControls;
end;

procedure TControlView.ToolButtons(const AControls: TToolButtons);
begin
  FToolButtons:= AControls;
end;

procedure TControlView.DialogButtons(const AControls: TToolButtons);
begin
  FDialogButtons:= AControls;
end;

procedure TControlView.ViewApply;
begin
  SetToolPanels;
  SetCaptionPanels;
  SetBorderPanels;

  SetDividerBevels;

  SetCategoryButtons;
  SetToolButtons;
  SetDialogButtons;
end;

end.

