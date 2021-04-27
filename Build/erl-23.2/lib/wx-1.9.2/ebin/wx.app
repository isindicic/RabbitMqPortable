%% This is an -*- erlang -*- file.
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

{application, wx,
 [{description, "Yet another graphics system"},
  {vsn, "1.9.2"},
  {modules,
   [
    %% Generated modules
  wxUpdateUIEvent, wxColourData, wxIcon, wxColourPickerEvent, wxGraphicsMatrix, wxImage, wxGraphicsContext, wxPreviewFrame, wxFontPickerCtrl, wxEvtHandler, wxFileDialog, wxFlexGridSizer, wxPrintDialogData, wxAuiNotebook, wxDisplay, wxDCOverlay, wxClipboardTextEvent, wxMoveEvent, wxChoicebook, wxSystemOptions, wxGridCellFloatRenderer, wxWindowDC, wxColourDialog, wxHtmlLinkEvent, wxStatusBar, wxInitDialogEvent, wxEraseEvent, wxXmlResource, wxToggleButton, wxTaskBarIconEvent, wxGraphicsObject, wxPrintout, wxSysColourChangedEvent, wxGridCellRenderer, wxListCtrl, wxLocale, wxBitmap, wxQueryNewPaletteEvent, wxSizerItem, wxDC, wxPasswordEntryDialog, wxFrame, wxNavigationKeyEvent, wxFontData, wxGraphicsRenderer, wxGridCellBoolRenderer, wxMouseCaptureLostEvent, wxTextEntryDialog, wxIdleEvent, wxListItem, wxSpinCtrl, wxMDIClientWindow, wxMDIChildFrame, wxStdDialogButtonSizer, wxPrintData, wxDirPickerCtrl, wxKeyEvent, wxEvent, wxFontDialog, wxRadioBox, wxMessageDialog, wxTreebook, wxSizeEvent, wxLogNull, wxPreviewCanvas, wxTextAttr, wxScrollWinEvent, wxCalendarCtrl, wxGraphicsBrush, wxWindowDestroyEvent, wxSetCursorEvent, wxMenuItem, wxChoice, wxGraphicsFont, wxStaticText, wxControl, wxJoystickEvent, wxPrinter, wxStaticBitmap, wxGridBagSizer, wxGridSizer, wxScrollEvent, wx_misc, wxWindowCreateEvent, wxGridCellFloatEditor, wxStyledTextEvent, wxPrintDialog, wxStaticBox, wxBufferedDC, wxTextCtrl, wxDropFilesEvent, wxMaximizeEvent, wxDateEvent, wxGridCellAttr, wxCalendarEvent, wxGauge, wxSizerFlags, wxGridCellTextEditor, wxDataObject, wxShowEvent, wxPrintPreview, wxFindReplaceDialog, wxGridCellStringRenderer, wxTextDataObject, wxPreviewControlBar, wxStaticLine, wxMiniFrame, wxListEvent, wxDialog, wxBrush, wxPaintDC, wxScreenDC, wxGraphicsPen, wxPopupWindow, wxChildFocusEvent, wxFilePickerCtrl, wxFindReplaceData, wxPostScriptDC, wxGrid, wxAuiSimpleTabArt, wxSashEvent, wxScrolledWindow, wxMask, wxSplitterEvent, wxScrollBar, wxMenuEvent, wxHtmlWindow, wxPaletteChangedEvent, wxIconBundle, wxListItemAttr, wxMirrorDC, wxAuiManager, wxBoxSizer, wxBitmapButton, wxMouseCaptureChangedEvent, wxClipboard, wxMouseEvent, wxDirDialog, wxMenu, wxAuiPaneInfo, wxPaintEvent, wxSplitterWindow, wxProgressDialog, wxCheckBox, wxListBox, wxActivateEvent, wxNotebookEvent, wxFileDirPickerEvent, wxCursor, wxButton, wxMenuBar, wxDisplayChangedEvent, wxToolBar, wxNotifyEvent, wxArtProvider, wxHtmlEasyPrinting, wxBufferedPaintDC, wxTreeCtrl, wxGridCellEditor, wxListView, wxAuiManagerEvent, wxGridCellNumberRenderer, wxNotebook, wxColourPickerCtrl, wxContextMenuEvent, wxLayoutAlgorithm, wxCheckListBox, wxTopLevelWindow, wxMultiChoiceDialog, wxOverlay, wxTaskBarIcon, wxAuiDockArt, wxComboBox, wxCommandEvent, wxPanel, wxSashWindow, wxBitmapDataObject, wxDatePickerCtrl, wxFocusEvent, wxGridCellChoiceEditor, wxListbook, wxImageList, wxToolTip, wxPalette, wxSlider, wxSizer, wxGBSizerItem, wxPen, wxFileDataObject, wxAuiNotebookEvent, wxGLCanvas, wxStaticBoxSizer, wxPageSetupDialogData, wxSplashScreen, wxMemoryDC, wxToolbook, wxPopupTransientWindow, wxGCDC, wxAcceleratorEntry, wxRadioButton, wxPickerBase, wxCloseEvent, wxCalendarDateAttr, wxCaret, wxIconizeEvent, wxAcceleratorTable, wxMDIParentFrame, wxHelpEvent, wxGridCellBoolEditor, wxGenericDirCtrl, wxFont, wxGridCellNumberEditor, wxControlWithItems, wxSystemSettings, wxWindow, wxTreeEvent, wxAuiTabArt, wxSpinEvent, wxSingleChoiceDialog, wxGraphicsPath, wxFontPickerEvent, wxRegion, wxClientDC, wxSpinButton, wxPageSetupDialog, wxSashLayoutWindow, wxStyledTextCtrl, wxGridEvent, glu, gl,
    %% Handcrafted modules
    wx,
    wx_object,
    wxe_master,
    wxe_server,
    wxe_util
   ]},
  {registered, []},
  {applications, [stdlib, kernel]},
  {env, []},
  {runtime_dependencies, ["stdlib-2.0","kernel-3.0","erts-6.0"]}
 ]}.
