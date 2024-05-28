unit DK_Filter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls;

const
  DELAY_MILLISECONDS_DEFAULT = 1;

type
  TFilterEvent = procedure(const AFilterString: String) of object;

  { TFilterForm }

  TFilterForm = class(TForm)
    FilterButton: TSpeedButton;
    FilterEdit: TEdit;
    FilterImages16: TImageList;
    FilterImages20: TImageList;
    FilterImages24: TImageList;
    FilterImages28: TImageList;
    FilterLabel: TLabel;
    FilterTimer: TTimer;
    procedure FilterButtonClick(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure FilterTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    OnFilterChange: TFilterEvent;
    CanApplyFilter: Boolean;
    CanStartTimer: Boolean;
  public

  end;

var
  FilterForm: TFilterForm;

  function CreateFilterControls(const ACaption: String;
        const APanel: TPanel;
        const AOnFilterChange: TFilterEvent;
        const AUpdateDelayMilliSeconds: Integer = DELAY_MILLISECONDS_DEFAULT): TFilterForm;

implementation

{$R *.lfm}

function CreateFilterControls(const ACaption: String;
        const APanel: TPanel;
        const AOnFilterChange: TFilterEvent;
        const AUpdateDelayMilliSeconds: Integer = DELAY_MILLISECONDS_DEFAULT): TFilterForm;
begin
  Result:= TFilterForm.Create(APanel);
  Result.FilterLabel.Caption:= ACaption;
  Result.Parent:= APanel;
  Result.Parent.Caption:= EmptyStr;
  Result.OnFilterChange:= AOnFilterChange;
  Result.FilterTimer.Interval:= AUpdateDelayMilliSeconds;
  Result.Show;
end;

{ TFilterForm }

procedure TFilterForm.FormShow(Sender: TObject);
begin
  FilterButton.Height:= FilterEdit.Height + 2;
  FilterButton.Width:= FilterButton.Height;

  case Screen.PixelsPerInch of
    96 : FilterButton.Images:= FilterImages16;
    120: FilterButton.Images:= FilterImages20;
    144: FilterButton.Images:= FilterImages24;
    168: FilterButton.Images:= FilterImages28;
  end;
end;

procedure TFilterForm.FilterTimerTimer(Sender: TObject);
begin
  FilterTimer.Enabled:= False;
  if not CanApplyFilter then Exit;
  if Assigned(OnFilterChange) then
    OnFilterChange(FilterEdit.Text);
end;

procedure TFilterForm.FilterEditChange(Sender: TObject);
begin
  FilterButton.Enabled:= FilterEdit.Text<>EmptyStr;
  if not CanStartTimer then Exit;

  CanApplyFilter:= False;
  if FilterTimer.Enabled then
    FilterTimer.Enabled:= False;
  FilterTimer.Enabled:= True;
  CanApplyFilter:= True;
end;

procedure TFilterForm.FilterButtonClick(Sender: TObject);
begin
  CanStartTimer:= False;
  FilterEdit.Text:= EmptyStr;
  CanStartTimer:= True;
  if Assigned(OnFilterChange) then
    OnFilterChange(FilterEdit.Text);
end;

procedure TFilterForm.FormCreate(Sender: TObject);
begin
  CanApplyFilter:= False;
  CanStartTimer:= True;
end;

end.

