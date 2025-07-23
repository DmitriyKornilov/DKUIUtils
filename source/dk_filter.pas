unit DK_Filter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, DK_StrUtils, UFilterImages;

const
  DELAY_MILLISECONDS_DEFAULT = 1;

type
  TFilterEvent = procedure(const AFilterString: String) of object;

  { TDKFilter }

  TDKFilter = class(TForm)
    FilterButton: TSpeedButton;
    FilterEdit: TEdit;
    FilterLabel: TLabel;
    FilterTimer: TTimer;
    procedure FilterButtonClick(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure FilterTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    OnFilterChange: TFilterEvent;
    CanApplyFilter: Boolean;
    CanStartTimer: Boolean;
    Images: TFilterImages;
  public

  end;

var
  DKFilter: TDKFilter;

  function DKFilterCreate(const ACaption: String;
        const APanel: TPanel;
        const AOnFilterChange: TFilterEvent;
        const AUpdateDelayMilliSeconds: Integer = DELAY_MILLISECONDS_DEFAULT): TDKFilter;

implementation

{$R *.lfm}

function DKFilterCreate(const ACaption: String;
        const APanel: TPanel;
        const AOnFilterChange: TFilterEvent;
        const AUpdateDelayMilliSeconds: Integer = DELAY_MILLISECONDS_DEFAULT): TDKFilter;
begin
  Result:= TDKFilter.Create(APanel);
  Result.FilterLabel.Caption:= ACaption;
  Result.Parent:= APanel;
  Result.Parent.Caption:= EmptyStr;
  Result.OnFilterChange:= AOnFilterChange;
  Result.FilterTimer.Interval:= AUpdateDelayMilliSeconds;
  Result.Show;
end;

{ TDKFilter }

procedure TDKFilter.FormCreate(Sender: TObject);
begin
  Images:= TFilterImages.Create(nil);
  CanApplyFilter:= False;
  CanStartTimer:= True;
end;

procedure TDKFilter.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Images);
end;

procedure TDKFilter.FormShow(Sender: TObject);
begin
  FilterButton.Height:= FilterEdit.Height + 2;
  FilterButton.Width:= FilterButton.Height;
  FilterButton.Images:= Images.ForScreenPPI;
end;

procedure TDKFilter.FilterTimerTimer(Sender: TObject);
begin
  FilterTimer.Enabled:= False;
  if not CanApplyFilter then Exit;
  if Assigned(OnFilterChange) then
    OnFilterChange(SUpper(FilterEdit.Text));
end;

procedure TDKFilter.FilterEditChange(Sender: TObject);
begin
  FilterButton.Enabled:= FilterEdit.Text<>EmptyStr;
  if not CanStartTimer then Exit;

  CanApplyFilter:= False;
  if FilterTimer.Enabled then
    FilterTimer.Enabled:= False;
  FilterTimer.Enabled:= True;
  CanApplyFilter:= True;
end;

procedure TDKFilter.FilterButtonClick(Sender: TObject);
begin
  CanStartTimer:= False;
  FilterEdit.Text:= EmptyStr;
  CanStartTimer:= True;
  if Assigned(OnFilterChange) then
    OnFilterChange(SUpper(FilterEdit.Text));
end;

end.

