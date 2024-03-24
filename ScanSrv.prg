// From Z:\Harbour\hb32\tests\ipsvr.prg 

#include "hbvo.ch"
#include "hbsocket.ch"

#define EOT		e"\x04"
#define ESC		e"\x1B"
#define ETX 		e"\x03"

// Команды протокола SG15
#define SG_CLEAR	e"\x1B\x24"
#define SG_CRLF		e"\x1B\x27\x30"		// Требует еще 1 байт - Номер строки (0x30 - 0x34)
#define SG_ALIGNR	e"\x1B\x2E\x3B"
#define SG_ALIGNBR	e"\x1B\x2E\x38"
#define SG_ALIGNCC	e"\x1B\x2E\x34"
#define SG_WRAPON	e"\x1B\x2D\x31"
#define SG_WRAPOFF	e"\x1B\x2D\x30"
#define SG_MINI		e"\x1B\x42\x33"
#define SG_SMALL	e"\x1B\x42\x30"
#define SG_LARGE	e"\x1B\x42\x31"


STATIC Path := ""			// Корень
STATIC Address := ""			// /ip=address:port
STATIC Port := 0
STATIC Timeout := 9000			// /timeout=9000 (=90 seconds) для sockets
STATIC TimeOutLock := 5			// TimeOut в секундах для мутексов
STATIC UpdAfter := 60			// /upd= Интервал Update в секундах
STATIC WPref := "29"			// /w=WPref:WPLU (ВесовойPrefix:ДлинаPLU)
STATIC WPLU := 6
STATIC IntCP				// /intCP= Внутренний cdp (Должен содержать "866" или "1251" [cdpOs]
STATIC TrmCP				// /trmCP= CP терминала (Должен содержать "UTF", иначе [cdpOs]
STATIC DbfCP				// /dbfCP= CP БД (Должен содержать "UTF", иначе [cdpOs]
STATIC TrmW := 240			// /trmw= Ширина экрана терминала в пикселях [240]
//STATIC TrmH := 128			// /trmh= Высота экрана терминала в пикселях [128]
STATIC TrmH := 5			// /trmh= Высота экрана терминала в СТРОКАХ !!!
STATIC TrmC := 0.85			// /trmс= Коэффициент для "маленького" фонта в пикселях относительно Arial18 
STATIC TrmMode := 0			// /trmmode= 	1	Выводить штрих-код на экран
					//		2	Не выравнивать название по краям
					//		4	Не умножать цену на количество в упаковке

STATIC ILog := 7			// /log=	0	Ничего не выводим
					//		1	Ошибки
					//		2	Основной протокол
					//		4	Трассировка важных сообщений
					//		8	Трассировка всех сообщений
					//		16	Трассировка импортируемых строк

STATIC lShutUp := .F.			// ShutUp
STATIC cLogFile := ""			// LOG
STATIC barcodeLen := NIL		// Длина поля баркода в vstWrk

STATIC hmutNew				// Mutex VstNew - обновляемой БД
STATIC hmutWrk				// Mutex Vst - рабочей БД


PROCEDURE Main()

	LOCAL niSys := 1			// Текущая версия программы
	LOCAL niRelease := 0			// Текущий релиз программы

	LOCAL cCmdLine := "", cCmdExe
	LOCAL i, j, c, oError
	LOCAL lastUpdate := Seconds(), tekSecs

	ERRORBLOCK({|errobj| ERRMACRO(errobj)})	// <HV>  Обработчик ошибок в макрах

	REQUEST HB_LANG_RU866		// Для HRB100 это и так работает
	REQUEST HB_LANG_RUWIN		// Для HRB300 нужна вся эта хрень,
	REQUEST HB_CODEPAGE_RU866	// при этом HB_LEGACY_LEVEL есть 
	REQUEST HB_CODEPAGE_RU1251	// только для HBR100
	IntCP := hb_cdpOs()
	hb_cdpSelect(IntCP)		// По умолчанию

	Path := hb_dirBase()

// ========== Командная строка =================================
	cCmdExe := Trim(hb_argv())
	cCmdLine := cCmdExe + " "
	FOR i:=1 TO PCount()			// Командная строка на запуск
		cCmdLine += Trim(Upper(HB_PValue(i))) + " "
	NEXT
	cCmdLine := StrTran(cCmdLine, "-", "/")
	cCmdLine := StrTran(cCmdLine, "/", " /") + " "
	IF !Empty(c := MySubstr(cCmdLine, "/IP= "))
		address := c
		IF (j := At(":", c)) > 0
			port := Val(Substr(c, j+1))
			address := Left(c, j-1) 
		ENDIF
	ENDIF

	IF !Empty(c := MySubstr(cCmdLine, "/W= "))	// Весовой товар
		WPref := c
		IF (j := At(":", c)) > 0
			WPLU := Val(Substr(c, j+1))
			WPref := Left(c, j-1) 
		ENDIF
	ENDIF

	IF !Empty(c := MySubstr(cCmdLine, "/TIMEOUT= "));	Timeout := Val(c);	ENDIF
	IF !Empty(c := MySubstr(cCmdLine, "/LOG= "));		ILog := Val(c);		ENDIF
	IF !Empty(c := MySubstr(cCmdLine, "/UPD= "));		UpdAfter := Val(c);	ENDIF
	IF !Empty(c := MySubstr(cCmdLine, "/TRMMODE= "));	TrmMode := Val(c);	ENDIF
	IF !Empty(c := MySubstr(cCmdLine, "/TRMW= "));		TrmW := Val(c);		ENDIF
	IF !Empty(c := MySubstr(cCmdLine, "/TRMH= "));		TrmH := Val(c);		ENDIF
	IF !Empty(c := MySubstr(cCmdLine, "/TRMC= "));		TrmC := Val(c);		ENDIF

	IF !Empty(c := MySubstr(cCmdLine, "/INTCP= "))
		IF "866" $ c;	IntCP := "RU866";	ENDIF
		IF "1251" $ c;	IntCP := "RU1251";	ENDIF
		hb_cdpSelect(IntCP)
	ENDIF
	TrmCP := IntCP
	IF !Empty(c := MySubstr(cCmdLine, "/TRMCP= "))
//		IF "866" $ c;	TrmCP := "RU866";	ENDIF
//		IF "1251" $ c;	TrmCP := "RU1251";	ENDIF
		IF "UTF" $ c;	TrmCP := "UTF8";	ENDIF
	ENDIF
	DbfCP := IntCP 
	IF !Empty(c := MySubstr(cCmdLine, "/DBFCP= "))
//		IF "866" $ c;	DbfCP := "RU866";	ENDIF
//		IF "1251" $ c;	DbfCP := "RU1251";	ENDIF
		IF "UTF" $ c;	DbfCP := "UTF8";	ENDIF
	ENDIF

	outLog(2, "ScanServer V.M " + NTrim(niSys) + "." + NTrim(niRelease) + " started at " + address + ":" + NTrim(port) + " in " + Path)	
	outLog(8, "W = '" + WPref + ":" + NTrim(WPLU) + "'")

	BEGIN SEQUENCE

// ========== Поехали =================================
		IF !hb_mtvm();	BREAK "multithread support required";	ENDIF

		hmutNew := hb_mutexCreate()		// mutex для vstNew
		hmutWrk := hb_mutexCreate()		// mutex для vstWrk

		IF !Init();	BREAK "initialisation error";		ENDIF
	        
		outLog(2, "create listening sockets (" + hb_cdpSelect() + ")")

		hb_threadDetach( hb_threadStart( @TCPScan() ) )	// TCP Listener

		DO WHILE .T.
			tekSecs := Seconds()

			IF tekSecs > lastUpdate + UpdAfter .OR. tekSecs < lastUpdate
				hb_threadDetach(hb_threadStart( @DbfUpd()))	// Обновление
				lastUpdate := Seconds()				// Время последнего обновления
			ENDIF

			IF Inkey() == K_ESC .OR. lShutup 
				outLog(2, "quitting - esc pressed")
				EXIT
			ENDIF
			hb_idleSleep(2)
		ENDDO

	RECOVER USING oError
		IF !EMPTY(oError)
			IF !Empty(c := SysError(oError, .F.));	outLog(1, "ScanSrv: " + c);	ENDIF
		ENDIF
	END

	outLog(2, "close sockets (" + NTrim(__vmCountThreads()) + ")")
	hb_threadTerminateAll()
//	outLog(2, "close database")
//	IF hb_dbRequest("vstWrk", .T.,, Timeout);	vstWrk->(DBCloseArea());	ENDIF	// На случай, если vstWrk открыта
	RETURN


// ========== Thread TCP Listener =================================
PROCEDURE TCPScan()

	LOCAL hListen
	LOCAL hSocket
	LOCAL u, u1, c, oError

	ERRORBLOCK({|errobj| ERRMACRO(errobj)})	// <HV>  Обработчик ошибок в макрах
	BEGIN SEQUENCE
		outLog(4, "create TCP listening socket")

		IF Empty( hListen := hb_socketOpen() );				BREAK "socket create error " + hb_ntos( hb_socketGetError());	ENDIF
		IF !hb_socketBind(hListen, {HB_SOCKET_AF_INET, address, port});	BREAK "bind error " + hb_ntos( hb_socketGetError());	ENDIF
		IF !hb_socketListen( hListen );					BREAK "listen error " + hb_ntos( hb_socketGetError());	ENDIF
		IF !(Valtype(u := hb_socketGetSockName( hListen )) = "A");	BREAK "socket error";					ENDIF

		outLog(2, "Listening TCP " + u[2] + ":" + NTrim(u[3]))	// hb_socketGetPeerName( hListen )) == NIL

		DO WHILE .T.
			IF Empty( hSocket := hb_socketAccept( hListen,, Timeout ) )
				IF hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
					outLog(8, "Timeout: loop")
				ELSE
					outLog(1, "accept error " + hb_ntos( hb_socketGetError() ))
				ENDIF
			ELSE
				u := hb_socketGetSockName( hSocket )
				u1 := hb_socketGetPeerName( hSocket )
				outLog(4, "accept socket request: Socket " + u[2] + ":" + NTrim(u[3]) + ", Peer " + u1[2] + ":" + NTrim(u1[3]))

				hb_threadDetach( hb_threadStart( @Scan(), hSocket ) )
			ENDIF
			IF Inkey() == K_ESC
				outLog(2, "quitting - esc pressed")
				EXIT
			ENDIF
		ENDDO

	RECOVER USING oError
		IF !EMPTY(oError)
			IF !Empty(c := SysError(oError, .F.));	outLog(1, "TCPScan: " + c);	ENDIF
		ENDIF
	END

	outLog(2, "close TCP listening socket")

	hb_socketShutdown( hListen )
	hb_socketClose( hListen )
	lShutup := .T.

	RETURN

// ========== Thread скана =================================
PROCEDURE Scan( hSocket )

	LOCAL j, k, c, cRequest, cAnswer, lFound, oError, cKey
	LOCAL nLen
	LOCAL cBuf
	LOCAL cBarcode, cNntName, cEdizm, rPrice, rUpak, rVes:=0, jLen, ch, chLen, cLine, cLineR, maxHName, iLine
	LOCAL arial18 := {	18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18, ;
				18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18, ;
				 7, 8, 9,13,13,21,16, 5, 8, 8, 9,14, 7, 8, 7, 7, ;
				13,13,13,13,13,13,13,13,13,13, 7, 7,14,14,14,13, ;
				24,15,16,17,17,16,15,19,17, 6,12,16,13,19,17,19, ;
				16,19,17,16,14,17,15,23,15,16,15, 7, 7, 7,12,13, ;
				 8,13,14,12,14,13, 7,14,14, 5, 6,12, 6,20,14,13, ;
				14,14, 8,12, 7,14,11,17,11,12,12, 8, 6, 8,14,18, ;
				21,13, 5, 9, 8,24,13,13,13,26,25, 8,24,14,22,17, ;
				14, 5, 5, 8, 8, 8,13,24,18,24,22, 8,20,11,14,13, ;
				 7,15,12,12,13,12, 6,13,16,18,17,13,14, 8,18, 6, ;
				10,13, 6, 5,10,14,13, 7,13,26,12,13, 6,16,12, 6, ;
				15,16,16,13,16,16,22,15,17,17,14,16,19,17,19,17, ;
				16,17,14,15,18,15,18,16,22,23,19,21,16,17,24,17, ;
				13,14,13, 9,14,13,16,11,13,13,11,14,17,13,13,13, ;
				14,12,11,12,20,11,14,13,19,20,15,17,13,12,18,13		}

	ERRORBLOCK({|errobj| ERRMACRO(errobj)})	// <HV>  Обработчик ошибок в макрах
	BEGIN SEQUENCE

		DO WHILE .T.
			cRequest := ""
			nLen := 1
			cBuf := Space( 4096 )
			DO WHILE hb_BAt( EOT, cRequest ) == 0 .AND. nLen > 0 .AND. !(CHR(10) $ cRequest)
				cBuf := Space( 4096 )
				IF ( nLen := hb_socketRecv( hSocket, @cBuf,,, Timeout ) ) > 0  /* Timeout */
					cRequest += hb_BLeft( cBuf, nLen )
				ELSE
					IF nLen == -1 .AND. hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
						nLen := 0
					ENDIF
				ENDIF
			ENDDO
        
			IF nLen == -1
				outLog(1, "recv() error:" + NTrim( hb_socketGetError() ))
			ELSEIF nLen == 0
				outLog(8, "connection closed")
				EXIT
			ELSE
				hb_socketSetKeepAlive( hSocket, .T. )
				hb_socketSetNoDelay( hSocket, .T. )
				
				IF "quit" $ cRequest
					outLog(4, "exit")
					EXIT
				ENDIF
        
				DO WHILE !Empty(cRequest) .AND. !IsDigit(Left(cRequest,1));	cRequest := Substr(cRequest,2);	ENDDO
				DO WHILE (j:=AtAny(e"\x0D\x0A\x03\x04", cRequest)) > 0;	cRequest := Left(cRequest,j-1);	ENDDO
				DO WHILE !Empty(cRequest) .AND. !IsDigit(Left(cRequest,1));	cRequest := Substr(cRequest,2);	ENDDO
				outLog(8, "request = '" + cRequest + "'")
        
				cAnswer := ""
				lFound := .F.
				IF hb_mutexLock(hmutWrk, Timeout)
					outLog(8, "requesting database")
					IF hb_dbRequest("vstWrk", .T.)			//,, TimeoutS)
						outLog(8, "search in database for " + cRequest)
						IF IsNil(barcodeLen)			// STATIC
							barcodeLen := vstWrk->(DBFieldInfo(DBS_LEN, FieldPos("BARCODE")))
						ENDIF
						vstWrk->(DBSetOrder(1))					// barcode
						vstWrk->(DBSeek(Padr(cRequest, barcodeLen)))		// barcode
						IF vstWrk->(EOF()) .AND. Len(cRequest)=13 .AND. cRequest = WPref	// Весовой?
							cKey := Padr(Padl(Substr(cRequest, Len(WPref)+1, WPLU), 6, "0"), barcodeLen)
							rVes := Val(Substr(cRequest, Len(WPref)+WPLU+1, 12-Len(WPref)-WPLU))/1000
							rVes := Round(rVes,3)
							outLog(8, "Поиск весового товара по barcode '" + cKey + "', вес=" + NTrim(rVes))
							IF !vstWrk->(DBSeek(cKey))	// barcode
								vstWrk->(DBSetOrder(2))	// nnt
								outLog(8, "Поиск весового товара по nnt '" + cKey + "', вес=" + NTrim(rVes))
								vstWrk->(DBSeek(cKey))
							ENDIF
						ENDIF

						IF vstWrk->(EOF())
							cAnswer := "Товар не найден"
						ELSE
							lFound := .T.
							cBarcode := AllTrim(vstWrk->barcode)
							cNntName := AllTrim(vstWrk->nntname)
							cEdizm   := AllTrim(vstWrk->ed_izm)
							rPrice   := vstWrk->price
							rUpak    := vstWrk->upak
							IF rVes # 0;	rUpak := rVes;	ENDIF
							IF DbfCP = "UTF"
								cNntName := hb_UTF8ToStr(cNntName)
								cEdizm   := hb_UTF8ToStr(cEdizm)
							ENDIF
						ENDIF
						
						hb_dbDetach("vstWrk")
					ELSE
						outLog(1, "failed to attach database")
					ENDIF
					hb_mutexUnLock(hmutWrk)
				ELSE
					outLog(1, "search lock timeout for " + cRequest)
				ENDIF
        
// Подготовка текста ответа -- зависит от терминала

/*
#define SG_CLEAR	e"\0x1B\0x24"
#define SG_ALIGNBR	e"\0x1B\0x2E\0x38"
#define SG_ALIGNCC	e"\0x1B\0x2E\0x34"
#define SG_WRAPON	e"\0x1B\0x2D\0x31"
#define SG_WRAPOFF	e"\0x1B\0x2D\0x30"
#define SG_SMALL	e"\0x1B\0x42\0x30"
#define SG_LARGE	e"\0x1B\0x42\0x31"
*/
				IF lFound
					cAnswer := SG_CLEAR + SG_SMALL

					iLine := 0
					maxHName := TrmH	// max число строк в названии
					IF rUpak = 0;	rUpak := 1;	ENDIF
					IF rUpak # 1;	maxHName--;	ENDIF	// Оставить 1 строку на упаковку
// Баркод
					IF IsSet(TrmMode, 1)					// Показывать баркод в 1 строке
						cAnswer := cBarcode + LF
						maxHName--
						iLine++
					ENDIF
// Строки названия
					jLen := 0				// Переносы строк в соответствии с 
					cLine := ""				// реальной шириной в пикселях
					cLineR := ""				// Остаток строки, сдвигаемой вправо
					FOR j:=1 TO Len(cNntName)
						ch := CharPos(cNntName, j)
						chLen := arial18[Asc(ch) + 1] * TrmC	// Ширина символа в пикселях
						IF jLen + chLen > TrmW			// Ширина ткущей строки в пикселях
							cLine := AllTrim(cLine)
							IF !IsSet(TrmMode,2) .AND.;	// Выравнивание краев абзаца, 
							   j < Len(cNntName) .AND.;	// кроме последней строки
							   maxHName > 1
								IF (k := AtAny(" ,;.:/\)]}-+", cLine)) > 0
									cLineR := AllTrim(Substr(cLine, k+1))
									cLine := AllTrim(Left(cLine,k))
								ENDIF
							ENDIF

							IF !Empty(cLine)		// Левая часть строки как есть
								IF TrmCP="UTF";	cLine := hb_strToUTF8(cLine);	ENDIF
								cAnswer += cLine
							ENDIF

							IF !Empty(cLineR)		// Правая часть строки прижата вправо
								IF TrmCP="UTF";	cLineR := hb_strToUTF8(cLineR);	ENDIF
								cAnswer += SG_ALIGNR + cLineR + ETX + SG_CRLF + Chr(0x30 + iLine + 1)
							ELSE
								cAnswer += LF
							ENDIF

							cLine := ""
							cLineR := ""
							jLen := 0
							maxHName--
							iLine++
							IF maxHName <= 0;	EXIT;	ENDIF
							IF ch = " ";		LOOP;	ENDIF	// Пробел в конце не переносим
						ENDIF
						cLine += ch
						jLen += chLen
					NEXT

					IF !Empty(cLine)	// Последняя строка
						IF TrmCP="UTF";	cLine := hb_strToUTF8(cLine);	ENDIF
						cAnswer += cLine + LF
					ENDIF
// Упаковка
					IF !IsSet(TrmMode,2) .AND. rUpak # 1
						cLine := ZTrim(rUpak) + " " + cEdizm + ;
							", цена за " + cEdizm + "=" + ZTrim(rPrice)
						IF TrmCP="UTF";	cLineR := hb_strToUTF8(cLineR);	ENDIF
//						cAnswer += SG_CRLF + Chr(0x33) + SG_MINI + cLine + LF
						cAnswer += SG_MINI + cLine + LF
						rPrice := Round(rPrice * rUpak,2)
					ENDIF
// Цена
					cLine := "Цена " + ZTrim(rPrice) + " руб."
					IF TrmCP = "UTF";	cLine := hb_strToUTF8(cLine);	ENDIF
					cAnswer += SG_LARGE + SG_ALIGNBR + cLine + ETX

//					cAnswer := Chr(0x1B) + Chr(0x24) + cRequest + ;
//					           Chr(0x1B) + Chr(0x25) + "Tomato" + ;
//					           Chr(0x1B) + Chr(0x2E) + Chr(0x38) + "3.62" + Chr(03) + Chr(03)
        
				ELSE
					IF Empty(cAnswer);	cAnswer := "Попробуйте еще раз";	ENDIF
					IF TrmCP = "UTF";	cAnswer := hb_strToUTF8(cAnswer);	ENDIF

					cAnswer := SG_CLEAR + SG_LARGE + SG_ALIGNCC + cAnswer + ETX
				ENDIF
        
//	//			hb_socketSend( hSocket, cBuffer, [ nLen = Len( cBuffer ) ], [ nFlags = 0 ], [ nTimeout = FOREVER ] ) --> nBytesSent
        
				nLen := hb_socketSend(hSocket, cAnswer + ETX,,, TIMEOUT)
				IF Len(cAnswer) > nLen
					outLog(1, "Bad Length: " + NTrim(Len(cAnswer)) + "/" + NTrim(nLen))
				ENDIF
        
				outLog(8, "answer = " + cAnswer)
			ENDIF
		ENDDO

	RECOVER USING oError
		IF !EMPTY(oError)
			IF !Empty(c := SysError(oError, .F.));	outLog(1, "Scan: " + c);	ENDIF
		ELSE
			outLog(8, "Scan: Just break")
		ENDIF
	END

	outLog(8, "close socket")

	IF Select("vstWrk") > 0; hb_dbDetach("vstWrk");	ENDIF
	hb_mutexUnLock(hmutWrk)
	hb_socketShutdown( hSocket )
	hb_socketClose( hSocket )

	RETURN

// ========== Log ======================================
STATIC FUNCTION outLog(iFlag, cMsg)		// Вывод сообщений
	LOCAL h
	IF IsSet(iFlag, iLog)
		? cMsg				// Пока так
		IF File(cLogFile)
			h := FOPEN(cLogFile, FO_READWRITE + FO_SHARED)
			FSEEK(h, 0, FS_END)
			cMsg := DTOC(DATE()) +" " + TIME() + " ScanSrv " + cMsg + CRLF
			FWRITE(h, cMsg)
			FCLOSE(h)
		ENDIF
	ENDIF
	RETURN .T.

// ========== Инициализация ======================================
STATIC FUNCTION Init()		// Инициализация
	LOCAL c, oError
	LOCAL cFile
	LOCAL aStru := {	{"NNT",		"C", 75, 0}, ;
				{"BARCODE",	"C", 15, 0}, ;
				{"NNTNAME",	"C", 200, 0}, ;
				{"PRICE",	"N", 15, 2}, ;
				{"ED_IZM",	"C", 10, 0}, ;
				{"UPAK",	"N", 9, 3}, ;
				{"PICTURE",	"C", 20, 0}	}

	#ifdef WVT
		REQUEST HB_GT_WVT
		REQUEST HB_GT_WVG
	#endif

	#ifdef ADS
		RDDRegister("ADS",1)
		ADSSetCharType(ADS_OEM, .T.)
		SET FILETYPE TO NTX
		ADSLocking(.T.)
	#endif
//	SetIns()					// Преобразование Shift-Ins в нормальный код
//	SETKEY(HB_KP_SHIFT_INS,	{|| ShiftIns()})	// Shift-Ins
//	SETKEY(K_CTRL_INS,	{|| CtrlIns()})		// Ctrl-Ins

//	SET SCOREBOARD OFF
//	SET TYPEAHEAD TO 512
	SET DELETED ON
	SET WRAP ON
	SET CONFIRM ON
	SET DATE BRITISH
	SET EPOCH TO 1980
//	SET PRINTER TO ("errdbf.txt") ADDITIVE
//	SET PRINTER ON
//	SET CONSOLE OFF
	SET( _SET_EVENTMASK, INKEY_ALL) 
	mSetCursor( .T. )

	BEGIN SEQUENCE

		hb_dirBuild(Path + "database")
		hb_dirBuild(Path + "exchange")
		hb_dirBuild(Path + "errlog")

		cLogFile := Path + "\errlog\errLog.txt"
		IF !File(cLogFile);	hb_memowrit(cLogFile, "");	ENDIF

		cFile := Path + "database\vstNew"
		IF !File(cFile + ".dbf")
			DBCREATE(cFile + ".dbf", aStru)
			USE (cFile) ALIAS vstNew NEW
			vstNew->(DBCREATEINDEX(cFile + "1.ntx", "barcode", &("{|| barcode}")))
			vstNew->(DBCREATEINDEX(cFile + "2.ntx", "nnt",	   &("{|| nnt}")))
			vstNew->(DBCLOSEAREA())
		ENDIF
		DbfUpd(.T.)	// !!! Здесь не thread !!! Обновление vstNew и принудительная перезапись в vstWrk, даже если ничего не менялось

	RECOVER USING oError
		IF !EMPTY(oError)
			IF !Empty(c := SysError(oError, .F.));	outLog(1, "Init: " + c);	ENDIF
		ENDIF
	END

	RETURN .T.

// ========== Копирование новой базы в старую ======================================
STATIC FUNCTION DbfReplace()			// Копирование Обновляемой vstNew в Рабочую vstWrk
	LOCAL r := .F.
	LOCAL cFileWrk := Path + "database\vstWrk"
	LOCAL cFileNew := Path + "database\vstNew"

//	IF hb_mutexLock(hmutNew, Timeout)		// Лочим vstNew. Она должна быть закрыта !!! Это делается в вызывающем модуле !!!
		IF hb_mutexLock(hmutWrk, Timeout)	// Лочим vstNew. Она должна быть закрыта
			IF hb_dbRequest("vstWrk", .T.)	// На случай, если vstWrk открыта
				vstWrk->(DBCloseArea())
			ENDIF
			outLog(8, "replace: Copy vstNew -> vstWrk")	
			FErase(cFileWrk + "*.*")
			FCopy(cFileNew + ".dbf", cFileWrk + ".dbf")
			FCopy(cFileNew + "1.ntx", cFileWrk + "1.ntx")
			FCopy(cFileNew + "2.ntx", cFileWrk + "2.ntx")
			DO WHILE !File(cFileWrk + ".dbf");	outLog(8, "Waiting for " + cFileWrk);	hb_IdleSleep(1);	ENDDO
        
			USE (cFileWrk) ALIAS vstWrk NEW
			vstWrk->(DBSetIndex(cFileWrk + "1.ntx"))
			vstWrk->(DBSetIndex(cFileWrk + "2.ntx"))
			outLog(8, "replace: vstWrk opened in " + NTrim(Select("vstWrk")))

			IF hb_dbDetach("vstWrk")
				outLog(8, "replace: vstWrk detached, wa = " + NTrim(Select("vstWrk")))
			ELSE
				outLog(1, "replace: failed to detach wrk database")
			ENDIF
			hb_mutexUnlock(hmutWrk)
			outLog(4, "replace: successful")	
			r := .T.
		ELSE
			outLog(1, "replace: failed to lock wrk database")
		ENDIF
//		hb_mutexUnlock(hmutNew)
//	ELSE
//		outLog(1, "replace: failed to lock new database")
//	ENDIF
	RETURN r

// ========== Обновление новой базы ================================================
STATIC FUNCTION DbfUpd(lForced)		// Обновление Обновляемой базы vstNew
	LOCAL r := .F., aDir, lChanged := .F., c, oError
	LOCAL cFileNew := Path + "database\vstNew"

	IF hb_mutexLock(hmutNew, Timeout)		// Лочим vstNew. Она должна быть закрыта

		ERRORBLOCK({|errobj| ERRMACRO(errobj)})	// <HV>  Обработчик ошибок в макрах
		BEGIN SEQUENCE

			IF IsNil(lForced);	lForced := .F.;	ENDIF	// Принудительная перезапись vstNew->vstWrk, даже если нет обновлений
			lChanged := lForced				// Были ли изменения
			outLog(8, "update: looking for updates")	
			USE (cFileNew) ALIAS vstNew NEW
			vstNew->(DBSetIndex(cFileNew + "1.ntx"))		// barcode
			vstNew->(DBSetIndex(cFileNew + "2.ntx"))		// nnt
        
			aDir := hb_Directory(Path + "exchange\*.*")		// Ищем обновления. F_DATE = TimeStamp !!!
			aDir := ASort(aDir,,, {|x,y| x[F_DATE] < y[F_DATE]})	// В порядке появления
        
			lChanged := lChanged .OR. DbfUpdScanport(aDir)		// Обновления Scanport
//			...
        
			IF lChanged
				vstNew->(__dbPack())
				vstNew->(DBReindex())
				outLog(8, "update: database reindexed")	
			ENDIF
			vstNew->(DBCloseArea())
			IF lChanged
				r := DBFReplace()
			ENDIF
        
		RECOVER USING oError
			IF !EMPTY(oError)
				IF !Empty(c := SysError(oError, .F.));	outLog(1, "DbfUpd: " + c);	ENDIF
			ENDIF
		END

		hb_mutexUnlock(hmutNew)	
	ELSE
		outLog(1, "update: failed to lock new database")
	ENDIF

	RETURN r

// ========== Обновление новой базы Scanport ==========================================
STATIC FUNCTION DbfUpdScanport(aDir)		// Обновление Обновляемой базы vstNew
	LOCAL i, j, k, c, cBuf, lChanged := .F., oError, cFileImp, h, af, ai, cNnt, cBarcode
	LOCAL aEmptyBars, lHasBars
	LOCAL cp:="UTF8"		// Кодировка файлов СканПорт по умолчанию

	ERRORBLOCK({|errobj| ERRMACRO(errobj)})	// <HV>  Обработчик ошибок в макрах
	BEGIN SEQUENCE

		IF AScan(aDir, {|x| "_BARCODES.DM" $ Upper(x[F_NAME])}) = 0 .AND. ;	// Нет ничего для ScanPort
		   AScan(aDir, {|x| "_ARTS.DM" $ Upper(x[F_NAME])}) = 0
			BREAK ""
		ENDIF
		lChanged := .T.
		FOR i:= 1 TO Len(aDir)
			cFileImp := Path + "exchange\" + Trim(aDir[i, F_NAME])
			outLog(4, "update: " + cFileImp)	
			IF (h := FOpen(cFileImp, FO_READ + FO_EXCLUSIVE)) = F_ERROR
				outLog(1, "update: Open error " + cFileImp)	
				LOOP
			ENDIF
			IF !IsNil(cBuf := FGetLine(h))					// Что в начале? Д.б. {+|-} [Кодировка]
				cBuf := Upper(cBuf)
				DO CASE
					CASE !(Left(cBuf,1) $ "+-")
					CASE "UTF" $ cBuf
					CASE "866" $ cBuf;	cp := "RU866"
					CASE "1251" $ cBuf;	cp := "RU1251"
				ENDCASE
				FSeek(h, 0, FS_SET)					// Обратно в начало
			ENDIF

			IF "_BARCODES.DM" $ Upper(cFileImp)
				DO WHILE !IsNil(cBuf := FGetLine(h))
					cBuf := MyTrim(cBuf)
					IF cp = "UTF" .AND. !(dbfCP = "UTF");	cBuf := hb_UTF8ToStr(cBuf);	ENDIF
					outLog(16, cBuf)	
					DO CASE
						CASE cBuf = "+"				// Обновление
						CASE cBuf = "-"				// Замена: Чистим все про баркоды
							vstNew->(DBSetOrder(0))
							vstNew->(DBGoTop())
							DO WHILE !vstNew->(EOF())
								vstNew->barcode :=""	// Чистим то, что из _barcodes
								IF Empty(vstNew->nntname) .AND. Empty(vstNew->barcode)
									vstNew->(DBDelete())
								ENDIF
								vstNew->(DBSkip(1))
							ENDDO
						OTHERWISE				// Новая строка
							ai := Piece(cBuf, 0, ";",, 1)	// Массив полей строки через ";", без кавычек
							cBarcode := ai[2]		// barcode
							cNnt := ai[5]			// nnt
							vstNew->(DBSetOrder(2))		// nnt	
							vstNew->(DBSeek(cNnt))		// Ищем первую встречную запись ННТ
							af := vstNew->(AScatter())	// Запомним первую встречную запись ННТ
							vstNew->(DBSetOrder(1))		// barcode
							vstNew->(DBSeek(cBarcode))	// Ищем Barcode
							IF vstNew->(EOF())		// Не нашли:
								vstNew->(DBAppend())	// Создаем и заполняем
								vstNew->(AGather(af))	// Реквизиты _arts - из первой встречной записи с тем же ННТ
							ENDIF
							vstNew->barcode	:= ai[2]	// Реквизиты _barcodes - из очередной строки
							vstNew->ed_izm	:= ai[3]
							vstNew->upak	:= Val(ai[4])
							vstNew->nnt	:= ai[5]
					ENDCASE
				ENDDO
			ENDIF

			IF "_ARTS.DM" $ Upper(cFileImp)
				DO WHILE !IsNil(cBuf := FGetLine(h))
					cBuf := MyTrim(cBuf)
					IF cp = "UTF" .AND. !(dbfCP = "UTF");	cBuf := hb_UTF8ToStr(cBuf);	ENDIF
					outLog(16, cBuf)	
					DO CASE
						CASE cBuf = "+"				// Обновление
						CASE cBuf = "-"				// Замена: Чистим все про баркоды
							vstNew->(DBSetOrder(0))
							vstNew->(DBGoTop())
							DO WHILE !vstNew->(EOF())
								vstNew->nntname :=""	// Чистим то, что из _arts
								IF Empty(vstNew->nntname) .AND. Empty(vstNew->barcode)
									vstNew->(DBDelete())
								ENDIF
								vstNew->(DBSkip(1))
							ENDDO
						OTHERWISE				// Новая строка
							ai := Piece(cBuf, 0, ";",, 1)	// Массив полей строки через ";", без кавычек
							cNnt := ai[2]			// nnt
							vstNew->(DBSetOrder(2))		// nnt
							vstNew->(DBSeek(cNnt))		// Ищем первую встречную запись ННТ
							IF vstNew->(EOF())		// Не нашли:
								vstNew->(DBAppend())	// Создаем и заполняем только ННТ
								vstNew->nnt := cNnt
							ENDIF

							aEmptyBars := {}		// Массив записей с пустыми баркодами
							lHasBars := .F.			// Есть непустые баркоды
							vstNew->(DBSeek(cNnt))		// Опять ищем первую встречную запись ННТ
							DO WHILE !vstNew->(EOF()) .AND. vstNew->nnt = cNnt
								vstNew->nntname	:= ai[3]	// Заносим реквизиты _arts ВО ВСЕ
								vstNew->price	:= Val(ai[4])	// записи с этим ННТ
								vstNew->picture	:= ai[5]
								IF Empty(vstNew->barcode)
									AAdd(aEmptyBars, vstNew->(RecNo()))
								ELSE
									lHasBars := .T.
								ENDIF
								vstNew->(DBSkip(1))
							ENDDO
							k := 1				// Удаляем все записи ННТ с пустым баркодом
							IF !lHasBars;	k := 2;	ENDIF	// Но если непустых нет вообще - одну пустую
							FOR j:=k TO Len(aEmptyBars)	// оставляем, мб позже приплывет баркод
								vstNew->(DBGoTo(aEmptyBars[j]))
								vstNew->(DBDelete())
							NEXT
					ENDCASE
				ENDDO
			ENDIF

			FClose(h)
			FErase(cFileImp)
		NEXT

	RECOVER USING oError
		IF !EMPTY(oError)
			IF !Empty(c := SysError(oError, .F.));	outLog(1, "DbfUpdScanport: " + c);	ENDIF
		ENDIF
	END
	RETURN lChanged

STATIC FUNCTION MyTrim(c)		// Отладочный Trim
	IF !(Valtype(c) == "C")
		outLog(1, "Wrong type " + Valtype(c))
		c := ""
	ENDIF
	RETURN Trim(c)

/*--------- ErrMacro ------------------------------------------------------25.12.93 ----------------*/
STATIC FUNCTION ERRMACRO(oError)	// Функция обработки ошибок в макрах
	LOCAL r:=NIL
	outLog(1, "!!! Error !!!")
	IF ValType(oError) = "O"
		oError:cargo := ErrMemo(oError)		// Чтобы получить трассировку из места возникновения
	ENDIF
	BREAK oError
	RETURN r

/*--------- ErrMemo -------------------------------------------------------09.01.95 ----------------*/
STATIC FUNCTION ErrMemo(Error)		// Функция выдачи расшифровки ошибки
	LOCAL i, j, c, h
	LOCAL err := ErrInf(Error)
	LOCAL cErrFile := Path + "errlog\errors.txt"

	IF !File(cErrFile);	hb_memowrit(cErrFile, "");	ENDIF
	h := FOpen(cErrFile, FO_READWRITE + FO_SHARED)
	FSeek(h, 0, FS_END)
//	SET PRINTER TO (cErrFile) ADDITIVE

// Общая информация об ошибке

	IF !Empty(Error:osCode)
		err += ";Ошибка DOS: " + LTRIM(STR(Error:osCode))
	ENDIF
	FSPut(h, err)

	c = ";Description: "+ Error:description;	FSPut(h, c);	err += PADR(c,30)
	c = " Filename   : "+ Error:filename;		FSPut(h, c);	err += PADR(c,30)
	c = ";Operation  : "+ Error:operation;		FSPut(h, c);	err += PADR(c,30)
	c = " GenCode    : "+ STR(Error:genCode);	FSPut(h, c);	err += PADR(c,30)
	c = ";OsCode     : "+ STR(Error:osCode);	FSPut(h, c);	err += PADR(c,30)
	c = " SubCode    : "+ STR(Error:subCode);	FSPut(h, c);	err += PADR(c,30)
	c = ";SubSystem  : "+ Error:SubSystem;		FSPut(h, c);	err += PADR(c,30)
	c = " Tries      : "+ STR(Error:tries);		FSPut(h, c);	err += PADR(c,30)
	c = ";Alias()    : "+ ALIAS()+STR(RECNO());	FSPut(h, c);	err += PADR(c,30)

// Стек

	FOR i:=1 TO 10
		IF (!Empty(ProcName(i)))
			c=PADR(TRIM(PROCNAME(i))+"("+LTRIM(STR(PROCLINE(i)))+")",25) + ;
			  IIF ((!Empty(ProcName(i+10))),	  		;
			  		PADR(TRIM(PROCNAME(i+10))+"("+;
						LTRIM(STR(PROCLINE(i+10)))+")",25),	;
					SPACE(30))
			FSPut(h, c)
			err += ";" + c
		ELSE
			EXIT
		ENDIF
	NEXT
	FClose(h)

 RETURN err

