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
    procedure Focus;
    procedure Clear(const ADoOnChangeEvent: Boolean = True);
  end;

var
  DKFilter: TDKFilter;

  function DKFilterCreate(const ACaption: String;
        const APanel: TPanel;
        const AOnFilterChange: TFilterEvent;
        const AWidth: Integer = -1;
        const AUpdateDelayMilliSeconds: Integer = DELAY_MILLISECONDS_DEFAULT;
        const AOnFilterKeyDown: TKeyEvent = nil): TDKFilter;

implementation

{$R *.lfm}

function DKFilterCreate(const ACaption: String;
        const APanel: TPanel;
        const AOnFilterChange: TFilterEvent;
        const AWidth: Integer = -1;
        const AUpdateDelayMilliSeconds: Integer = DELAY_MILLISECONDS_DEFAULT;
        const AOnFilterKeyDown: TKeyEvent = nil): TDKFilter;
begin
  Result:= TDKFilter.Create(APanel);
  Result.Parent:= APanel;
  Result.Parent.Caption:= EmptyStr;
  if AWidth>0 then
  begin
    Result.Parent.AutoSize:= False;
    Result.Parent.Width:= AWidth;
  end
  else
    Result.Parent.AutoSize:= True;
  Result.FilterLabel.Caption:= ACaption;
  Result.OnFilterChange:= AOnFilterChange;
  if Assigned(AOnFilterKeyDown) then
    Result.FilterEdit.OnKeyDown:= AOnFilterKeyDown;
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
  FilterButton.Height:= FilterEdit.Height;
  {$IFDEF WINDOWS}
  FilterButton.Height:= FilterButton.Height + 2;
  {$ENDIF}
  FilterButton.Width:= FilterButton.Height;
  FilterButton.Images:= Images.ForScreenPPI;
end;

procedure TDKFilter.Focus;
begin
  FilterEdit.SetFocus;
end;

procedure TDKFilter.Clear(const ADoOnChangeEvent: Boolean = True);
begin
  CanStartTimer:= False;
  FilterEdit.Text:= EmptyStr;
  CanStartTimer:= True;
  if ADoOnChangeEvent and Assigned(OnFilterChange) then
    OnFilterChange(SUpper(FilterEdit.Text));
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
  Clear;
end;

end.

