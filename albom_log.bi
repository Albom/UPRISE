

Enum ErrorType '���� ������
	ErrorLib						' ������ ���� "���������� �� �������"
	ErrorFunc					' ������ ���� "������� �� �������"
	ErrorSeansNotLoaded		' �� ���� ����� �� ��������
	ErrorNotEnoughMemory		' �� ������� ������
	ErrorRazrNotLoaded		' ���� ���������� �� ��������
	ErrorNoOutputFiles		' ��� ������ ��� ������
	ErrorLoadASFile			' ������ �������� AS-�����
	ErrorInputData			 	' �������� ������� ������
	ErrorFortranLib			' ���������� ��������� ���������� ���
	ErrorAFunction				' ������ ���
	ErrorFilter					' ������ ����� �������
	ErrorConfig					' ������ ����� �������
	ErrorFlip					' ������ �������� ������ FLIP
End Enum



Sub PrintErrorToLog( _ 
					ByVal	ErrNum		As	Integer, _
					ByVal	FileName	As String, _
					ByVal	StrNum	As Integer _
					)

	Dim err_message As ZString * 256 '������ � ��������� ������
	Dim FileHandle As Integer '���������� ����� 
	
	Select Case ErrNum
		Case ErrorLib
			err_message = "������ �������� ������������ ����������"
		Case ErrorFunc
			err_message = "������ �������� �������"
		Case ErrorSeansNotLoaded
			err_message = "�� ���� ����� �� ��������"
		Case ErrorNotEnoughMemory
			err_message = "�� ������� ������"
		Case ErrorRazrNotLoaded
			err_message = "���� ���������� �� ��������"
		Case ErrorNoOutputFiles
			err_message = "��� ������ ��� ������"
		Case ErrorLoadASFile
			err_message = "������ �������� AS-, BS- ��� CS- �����"
		Case ErrorInputData
			err_message = "�������� ������� ������"
		Case ErrorFortranLib
			err_message = "���������� ��������� ���������� ���"
		Case ErrorAFunction
			err_message = "������ � ��������� ������� ��������������� (���)"
		Case ErrorFilter
			err_message = "������ �������� ����� ��� �� �������"
		Case ErrorConfig
			err_message = "�� ������� ������� ���������������� ����"
		Case ErrorFlip
			err_message = "������ �������� ������ �� ������ FLIP"
	End Select
	FileHandle = FreeFile '����������� ����������� ��������� �������� 
	Open "Error_Log_UPRISE.txt" For Append As #FileHandle
	Print #FileHandle, Date; ", "; Time; ". "; err_message; ". ����: ";  Mid(FileName, InStrRev(FileName, "\")+1); ". ������: "; StrNum; ". ��������� �������: "; __DATE__
	Close #FileHandle

End Sub
