unit DK_ColorLegend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, DK_Vector;

const
  COLORPANEL_DEFAULT_WIDTH = 25;

type

  { TColorLegend }

  TColorLegend = class (TWinControl)
  private
    FParent: TPanel;
    FPanels: array of TPanel;
    FLabels: array of TLabel;

    procedure PanelAdd;
    procedure LabelAdd;
  public
    constructor {%H-}Create(const APanel: TPanel);
    procedure Legend(const AColors: TColorVector; const AValues: TStrVector);
  end;

function ColorLegendCreate(const APanel: TPanel;
                           const AColors: TColorVector;
                           const AValues: TStrVector): TColorLegend;

implementation

function ColorLegendCreate(const APanel: TPanel;
                           const AColors: TColorVector;
                           const AValues: TStrVector): TColorLegend;
begin
  Result:= TColorLegend.Create(APanel);
  Result.Legend(AColors, AValues);
end;

{ TColorLegend }

constructor TColorLegend.Create(const APanel: TPanel);
begin
  inherited Create(APanel);
  APanel.AutoSize:= True;
  APanel.Caption:= EmptyStr;

  FParent:= TPanel.Create(APanel);
  FParent.Parent:= APanel;
  FParent.Align:= alLeft;
  FParent.AutoSize:= True;
  FParent.Color:= APanel.Color;
  FParent.BevelInner:= bvNone;
  FParent.BevelOuter:= bvNone;
  FParent.BorderStyle:= bsNone;
end;

procedure TColorLegend.PanelAdd;
var
  Index: Integer;
  P: TPanel;
begin
  Index:= Length(FPanels);

  P:= TPanel.Create(FParent);
  P.Parent:= FParent;
  P.BevelColor:= clBlack;
  P.BevelInner:= bvSpace;
  P.BevelOuter:= bvNone;
  P.BorderStyle:= bsNone;
  P.Width:= FParent.Scale96ToScreen(COLORPANEL_DEFAULT_WIDTH);

  P.Align:= alCustom;
  P.AnchorParallel(akTop, 2, FParent);
  P.AnchorParallel(akBottom, 2, FParent);
  if Index=0 then
    P.AnchorParallel(akLeft, 2, FParent)
  else
    P.AnchorToNeighbour(akLeft, 20, FLabels[Index-1]);

  SetLength(FPanels, Index+1);
  FPanels[Index]:= P;
end;

procedure TColorLegend.LabelAdd;
var
  Index: Integer;
  L: TLabel;
begin
  Index:= Length(FLabels);

  L:= TLabel.Create(FParent);
  L.Parent:= FParent;
  L.AutoSize:= True;
  L.Color:= FParent.Color;

  L.Align:= alCustom;
  L.AnchorVerticalCenterTo(FPanels[Index]);
  L.AnchorToNeighbour(akLeft, 6, FPanels[Index]);

  SetLength(FLabels, Index+1);
  FLabels[Index]:= L;
end;

procedure TColorLegend.Legend(const AColors: TColorVector; const AValues: TStrVector);
var
  i, OldSize, NewSize: Integer;
begin
  OldSize:= Length(FPanels);
  NewSize:= Length(AColors);

  if NewSize<OldSize then
  begin
    for i:= NewSize to OldSize-1 do
    begin
      FreeAndNil(FPanels[i]);
      FreeAndNil(FLabels[i]);
    end;
  end
  else if NewSize>OldSize then
  begin
    for i:= OldSize+1 to NewSize do
    begin
      PanelAdd;
      LabelAdd;
    end;
  end;

  for i:= 0 to NewSize-1 do
  begin
    FPanels[i].Color:= AColors[i];
    FLabels[i].Caption:= AValues[i];
  end;
end;

end.

