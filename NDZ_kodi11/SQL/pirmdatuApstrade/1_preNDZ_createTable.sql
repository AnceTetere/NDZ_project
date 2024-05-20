USE [NDZ]
GO

--Sākumā aiļu platumu neskatos, to izformēs pēc tam.
--NDZ dati nāk ar daudzām tukšām rindām, tās jāiztīra.
--Sāku ar tabulu preNDZ_20XX tabulu, kuru pēcāk izdzēsīšu.
CREATE TABLE preNDZ_2021 (
	NM_code varchar(300),
	NMR_ATB_PK_ALG varchar(5),
	DN_code varchar(300),
	DN_ATB_PK_ALG varchar(300),
	sandat varchar(300),
	zinkod varchar(300),
	valsts varchar(300),
	prof varchar(300),
	[period] varchar(300));
