{
	MIT License

	Copyright (c) 2025 TheOnlyASDK

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.

}

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, LCLIntf, LCLProc, Buttons, ShellApi, RegExpr, AboutForm;


type

  { TFMain }

  TFMain = class(TForm)
    BBInfo: TBitBtn;
    BChooseFile: TButton;
    BChooseTool: TButton;
    BConvert: TButton;
    BDownloadTool: TButton;
    BBGitHub: TBitBtn;
    CBOpenAfterConversion: TCheckBox;
    CBSeperateComponents: TCheckBox;
    CB16BitPng: TCheckBox;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    TBFilename: TEdit;
    Label1: TLabel;
    TBPathToTool: TEdit;
    TBIterations: TTrackBar;
    TBThreadCount: TTrackBar;
    procedure BBGitHubClick(Sender: TObject);
    procedure BBInfoClick(Sender: TObject);
    procedure BChooseFileClick(Sender: TObject);
    procedure BChooseToolClick(Sender: TObject);
    procedure BConvertClick(Sender: TObject);
    procedure BDownloadToolClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TBIterationsChange(Sender: TObject);
    procedure TBThreadCountChange(Sender: TObject);
  private
    procedure AutoDetectTool;
    function IsValidJpeg2PngExecutable(const FileName: string): Boolean;
    procedure SetControlsEnabled(State: Boolean);
  public

  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

function TFMain.IsValidJpeg2PngExecutable(const FileName: string): Boolean;
var
  Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^jpeg2png(_[\d\.]+)?_(x64|x86)?\.exe$';
    Result := Regex.Exec(ExtractFileName(FileName));
  finally
    Regex.Free;
  end;
end;

procedure TFMain.SetControlsEnabled(State: Boolean);
begin
     TBFilename.Enabled:=State;
     TBPathToTool.Enabled:=State;
     BChooseFile.Enabled:=State;
     BChooseTool.Enabled:=State;
     BConvert.Enabled:=State;
     BDownloadTool.Enabled:=State;
     TBIterations.Enabled:=State;
     TBThreadCount.Enabled:=State;
     CBSeperateComponents.Enabled:=State;
     CB16BitPng.Enabled:=State;
     CBOpenAfterConversion.Enabled:=State;
end;

procedure TFMain.BConvertClick(Sender: TObject);
var
  ExePath, FilePath, OutputFile, OutputLine, OutputLineModified, Match, CleanMatch: string;
  Process: TProcess;
  OutputStream: TMemoryStream;
  OutputBuffer: array[0..1023] of Char;
  BytesRead: LongInt;
  UserChoice: Integer;
  ErrorLine: string;
  CommandLine: string;
  RegExp: TRegExpr;
  ToolFileName: string;
begin
  if Trim(TBPathToTool.Text) = '' then
  begin
    MessageDlg('Please enter or choose a valid path to the tool.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if Trim(TBFilename.Text) = '' then
  begin
    MessageDlg('Please enter or choose a file.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if not DirectoryExists(ExtractFilePath(TBPathToTool.Text)) or not FileExists(TBPathToTool.Text) then
  begin
    MessageDlg('The specified path to tool does not exist.', mtError, [mbOK], 0);
    Exit;
  end;

  if not FileExists(TBFilename.Text) then
  begin
    ShowMessage('The specified filename does not exist.');
    Exit;
  end;

  ExePath := TBPathToTool.Text;
  FilePath := TBFilename.Text;
  OutputFile := ChangeFileExt(FilePath, '.png');
  ToolFileName := ExtractFileName(ExePath);

  if not IsValidJpeg2PngExecutable(ToolFileName) then begin
    MessageDlg('Not a valid jpeg2png executable! Check the filename of your executable again!', mtError, [mbOK], 0);
    Exit;
  end;

  if FileExists(OutputFile) then
  begin
    UserChoice := MessageDlg('A PNG file with the same name already exists. Do you want to delete it and continue?', mtConfirmation, [mbYes, mbNo], 0);
    if UserChoice = mrNo then
    begin
      ShowMessage('Conversion canceled.');
      Exit;
    end
    else
      DeleteFile(OutputFile);
  end;

  SetControlsEnabled(false);

  Process := TProcess.Create(nil);
  try
    CommandLine := ExePath + ' ' + sLineBreak;
    Process.Executable := ExePath ;

    CommandLine := CommandLine + FilePath + ' ';
    Process.Parameters.Add(FilePath);

    CommandLine := CommandLine + ' --threads ' + IntToStr(TBThreadCount.Position);
    Process.Parameters.Add('--threads');
    Process.Parameters.Add(IntToStr(TBThreadCount.Position));

    CommandLine := CommandLine + ' --iterations ' + IntToStr(TBIterations.Position);
    Process.Parameters.Add('--iterations');
    Process.Parameters.Add(IntToStr(TBIterations.Position));

    if CB16BitPng.Checked then begin
          CommandLine := CommandLine + ' --16-bits-png ';
          Process.Parameters.Add('--16-bits-png');
    end;

    if CBSeperateComponents.Checked then begin
          CommandLine := CommandLine + ' --16-bits-png ';
          Process.Parameters.Add('--16-bits-png');
    end;

    ShowMessage('Command Line:'#13#10 + CommandLine);

    Process.Options := [poUsePipes, poNoConsole];
    Process.Execute;

    OutputStream := TMemoryStream.Create;
    ErrorLine := '';
    try
      repeat
        BytesRead := Process.Output.Read(OutputBuffer, SizeOf(OutputBuffer) - 1);
        if BytesRead > 0 then
        begin
          OutputBuffer[BytesRead] := #0;
          OutputLine := String(OutputBuffer);

          RegExp := TRegExpr.Create('\[(#+)?(\s+)?\]\s+(\d+%)');
          try
            OutputLineModified := RegExp.Replace(OutputLine, '$3 $1', True);
          finally
            RegExp.Free;
          end;

          if Pos('Error', OutputLine) > 0 then
            ErrorLine := ErrorLine + OutputLineModified + #13#10
          else
            StatusBar.SimpleText := OutputLineModified;

          Application.ProcessMessages;
        end;
      until BytesRead = 0;
    finally
      OutputStream.Free;
    end;

    Process.WaitOnExit;

    if Process.ExitStatus = 0 then
    begin
      StatusBar.SimpleText := 'Conversion successful!';
      if CBOpenAfterConversion.Checked then
      begin
        ShellExecute(0, 'open', PChar(FilePath), nil, nil, 1);
        ShellExecute(0, 'open', PChar(OutputFile), nil, nil, 1);
      end;
    end
    else
    begin
      StatusBar.SimpleText := 'Conversion failed. Exit code: ' + IntToStr(Process.ExitStatus);

      if ErrorLine <> '' then
        ShowMessage('Conversion Errors:'#13#10 + ErrorLine)
      else
        ShowMessage('Conversion failed. Exit code: ' + IntToStr(Process.ExitStatus));
    end;

  finally
    Process.Free;
    SetControlsEnabled(true);
  end;
end;

procedure TFMain.BDownloadToolClick(Sender: TObject);
var
  Arch, Msg: string;
begin
  {$IFDEF CPU64}
  Arch := '64-bit';
  {$ELSE}
  Arch := '32-bit';
  {$ENDIF}

  if MessageDlg('Download jpeg2png',
    'To download jpeg2png, click OK. This will take you to the download page.',
    mtInformation, [mbOK, mbCancel], 0) = mrOk then
  begin
    OpenURL('https://github.com/ThioJoe/jpeg2png/releases');

    Msg := 'Steps to download and use the tool:'#13#10 +
           '1. Download the correct version for your system (' + Arch + ').'#13#10 +
           '2. Save it in a known folder.'#13#10 +
           '3. Click the "Choose" button next to "Path to jpeg2png".'#13#10 +
           '4. Select the downloaded EXE file.';

    ShowMessage(Msg);
  end;

  AutoDetectTool;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  TBThreadCount.Position := GetCPUCount;

  Constraints.MinWidth:=Width;
  Constraints.MinHeight:=Height;

  AutoDetectTool;
end;

procedure TFMain.TBIterationsChange(Sender: TObject);
begin
     StatusBar.SimpleText:=Concat('Iterations: ', IntToStr(TBIterations.Position));
end;

procedure TFMain.TBThreadCountChange(Sender: TObject);
begin
  StatusBar.SimpleText:=Concat('Threads: ', IntToStr(TBThreadCount.Position));
end;

procedure TFMain.BChooseFileClick(Sender: TObject);
begin
  OpenDialog.Filter := 'JPEG files (*.jpg;*.jpeg)|*.jpg;*.jpeg|All files (*.*)|*.*';
  if OpenDialog.Execute then
  begin
    if FileExists(OpenDialog.FileName) then
      TBFilename.Text := OpenDialog.FileName
    else
      ShowMessage('The selected file does not exist.');
  end;
end;

procedure TFMain.BBGitHubClick(Sender: TObject);
begin
     OpenURL('https://github.com/theonlyasdk/jpeg-artifact-remover');
end;

procedure TFMain.BBInfoClick(Sender: TObject);
begin
     FAbout.ShowModal;
end;

procedure TFMain.BChooseToolClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Executable files (*.exe)|*.exe|All files (*.*)|*.*';
  if OpenDialog.Execute then
  begin
    if FileExists(OpenDialog.FileName) then
      TBPathToTool.Text := OpenDialog.FileName
    else
      ShowMessage('The selected file does not exist.');
  end;
end;

procedure TFMain.AutoDetectTool;
var
  SearchPaths: array of string;
  I: Integer;
  ExeName: string;
  Regex: TRegExpr;
  SR: TSearchRec;
begin
  SetLength(SearchPaths, 3);
  SearchPaths[0] := GetEnvironmentVariable('USERPROFILE') + '\Downloads\';
  SearchPaths[1] := GetEnvironmentVariable('USERPROFILE') + '\Documents\';
  SearchPaths[2] := GetEnvironmentVariable('USERPROFILE') + '\Desktop\';

  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^jpeg2png(_[\d\.]+)?_(x64|x86)?\.exe$';
    for I := 0 to High(SearchPaths) do
    begin
      if DirectoryExists(SearchPaths[I]) then
      begin

        if FindFirst(SearchPaths[I] + '*.exe', faAnyFile, SR) = 0 then
        begin
          repeat
            if Regex.Exec(SR.Name) then
            begin
              TBPathToTool.Text := SearchPaths[I] + SR.Name;
              Exit;
            end;
          until FindNext(SR) <> 0;
          FindClose(SR);
        end;
      end;
    end;
  finally
    Regex.Free;
  end;
end;

end.

