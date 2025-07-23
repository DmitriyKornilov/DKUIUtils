unit DK_DropFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  BCButton, ExtCtrls, UFilterImages, DK_VSTDropDown, DK_Vector;

type
  TDropFilterEvent = procedure(const AFilterIndex: Integer) of object;

  { TDKDropFilter }

  TDKDropFilter = class(TForm)
    FilterButton: TSpeedButton;
    FilterBCButton: TBCButton;
    FilterLabel: TLabel;
    procedure FilterButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    OnFilterChange: TDropFilterEvent;
    Images: TFilterImages;
    FilterDropDown: TVSTDropDown;
    procedure FilterChange;
  public
    procedure SetItems(const AItems: TStrVector);
  end;

var
  DKDropFilter: TDKDropFilter;

  function DKDropFilterCreate(const ACaption: String;
                        const APanel: TPanel;
                        const AOnFilterChange: TDropFilterEvent): TDKDropFilter;

implementation

{$R *.lfm}

function DKDropFilterCreate(const ACaption: String;
                        const APanel: TPanel;
                        const AOnFilterChange: TDropFilterEvent): TDKDropFilter;
begin
  Result:= TDKDropFilter.Create(APanel);
  Result.FilterLabel.Caption:= ACaption;
  Result.Parent:= APanel;
  Result.Parent.Caption:= EmptyStr;
  Result.OnFilterChange:= AOnFilterChange;
  Result.Show;
end;

{ TDKDropFilter }

procedure TDKDropFilter.FormCreate(Sender: TObject);
begin
  Images:= TFilterImages.Create(nil);

  FilterDropDown:= TVSTDropDown.Create(FilterBCButton);
  FilterDropDown.DropDownCount:= 20;
  FilterDropDown.OnChange:= @FilterChange;
end;

procedure TDKDropFilter.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Images);
  FreeAndNil(FilterDropDown);
end;

procedure TDKDropFilter.FormShow(Sender: TObject);
begin
  FilterButton.Height:= FilterBCButton.Height + 2;
  FilterButton.Width:= FilterButton.Height;
  FilterButton.Images:= Images.ForScreenPPI;
end;

procedure TDKDropFilter.FilterChange;
begin
  FilterButton.Enabled:= FilterDropDown.ItemIndex>0;
  if Assigned(OnFilterChange) then
    OnFilterChange(FilterDropDown.ItemIndex);
end;

procedure TDKDropFilter.SetItems(const AItems: TStrVector);
begin
  if VIsNil(AItems) then
    FilterDropDown.Clear
  else begin
    FilterDropDown.Items:= AItems;
    FilterDropDown.ItemIndex:= 0;
  end;
end;

procedure TDKDropFilter.FilterButtonClick(Sender: TObject);
begin
  if VIsNil(FilterDropDown.Items) then Exit;
  FilterDropDown.ItemIndex:= 0;
end;

end.

