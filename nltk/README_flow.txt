RAW DATA:
	2500638,"Thank you for using the Schaeffler Group IT Service Desk. Your request has been completed. Please try again. If you require further assistance, please contact the Schaeffler Group IT Service Desk. Phone Ext: x1188 or 1-888-462-6811 Self Service: http://tsd.de.ina.com/magicsshd E-mail: it-support-sg-na@schaeffler.com Thank you, Rodrigo Melendez Schaeffler Service Desk, North America _____________________________________________ From: Sevel, Evan NI/IBC-SIM Sent: Wednesday, April 11, 2012 12:13 AM To: IT-Support Schaeffler-Group North America Subject: 500 Internal Server Error When trying to access the Self Service to access my benefits and payment, I receive the error “500 Internal Server Error.” Evan P. Sevel International Key Account Manager Schaeffler Group USA Inc. Mobile: 704-519-8474 E-mail: sevelean@schaeffler.com Catalog: http://medias.schaeffler.com/medias/en!hp/","--melenrdr - 04/11/2012 - 06:53 13-------------- _____________________________________________ From: Sevel, Evan NI/IBC-SIM Sent: Wednesday, April 11, 2012 12:13 AM To: IT-Support Schaeffler-Group North America Subject: 500 Internal Server Error When trying to access the Self Service to access my benefits and payment, I receive the error “500 Internal Server Error.” Evan P. Sevel International Key Account Manager Schaeffler Group USA Inc. Mobile: 704-519-8474 E-mail: sevelean@schaeffler.com Catalog: http://medias.schaeffler.com/medias/en!hp/",--melenrdr - 04/11/2012 - 06:53 21-------------- SAP HP1 account validity date has been extended. Problem resolved. ---------------------------------------------------

	REFERENCE NUMBER:
		2500638

	USER INFORMATION:
		"Thank you for using the Schaeffler Group IT Service Desk. Your request has been completed. Please try again. If you require further assistance, please contact the Schaeffler Group IT Service Desk. Phone Ext: x1188 or 1-888-462-6811 Self Service: http://tsd.de.ina.com/magicsshd E-mail: it-support-sg-na@schaeffler.com Thank you, Rodrigo Melendez Schaeffler Service Desk, North America _____________________________________________ From: Sevel, Evan NI/IBC-SIM Sent: Wednesday, April 11, 2012 12:13 AM To: IT-Support Schaeffler-Group North America Subject: 500 Internal Server Error When trying to access the Self Service to access my benefits and payment, I receive the error “500 Internal Server Error.” Evan P. Sevel International Key Account Manager Schaeffler Group USA Inc. Mobile: 704-519-8474 E-mail: sevelean@schaeffler.com Catalog: http://medias.schaeffler.com/medias/en!hp/"

	INCIDENT PROBLEM:
		"--melenrdr - 04/11/2012 - 06:53 13-------------- _____________________________________________ From: Sevel, Evan NI/IBC-SIM Sent: Wednesday, April 11, 2012 12:13 AM To: IT-Support Schaeffler-Group North America Subject: 500 Internal Server Error When trying to access the Self Service to access my benefits and payment, I receive the error “500 Internal Server Error.” Evan P. Sevel International Key Account Manager Schaeffler Group USA Inc. Mobile: 704-519-8474 E-mail: sevelean@schaeffler.com Catalog: http://medias.schaeffler.com/medias/en!hp/"

	INCIDENT SOLUTION:
		--melenrdr - 04/11/2012 - 06:53 21-------------- SAP HP1 account validity date has been extended. Problem resolved. ---------------------------------------------------


AFTER PREPROCESSOR (A AND) B:
	2500638,"   try again.       _____________________________________________ From: Sevel, Evan NI/IBC-SIM Sent: Wednesday, April 11, 2012 12:13 AM  Subject: 500 Internal Server Error When trying to access the Self Service to access my benefits and payment, I receive the error “500 Internal Server Error.” Evan P. Sevel International Key Account Manager  Mobile: 704-519-8474 E-mail: sevelean@schaeffler.com Catalog: http://medias.schaeffler.com/medias/en!hp/","-- - 04/11/2012 - 06:53 13-------------- _____________________________________________ From: Sevel, Evan NI/IBC-SIM Sent: Wednesday, April 11, 2012 12:13 AM  Subject: 500 Internal Server Error When trying to access the Self Service to access my benefits and payment, I receive the error “500 Internal Server Error.” Evan P. Sevel International Key Account Manager  Mobile: 704-519-8474 E-mail: sevelean@schaeffler.com Catalog: http://medias.schaeffler.com/medias/en!hp/",-- - 04/11/2012 - 06:53 21-------------- SAP HP1 account validity date has been extended.  ---------------------------------------------------

	REFERENCE NUMBER:
		2500638

	USER INFORMATION:
		"   try again.       _____________________________________________ From: Sevel, Evan NI/IBC-SIM Sent: Wednesday, April 11, 2012 12:13 AM  Subject: 500 Internal Server Error When trying to access the Self Service to access my benefits and payment, I receive the error “500 Internal Server Error.” Evan P. Sevel International Key Account Manager  Mobile: 704-519-8474 E-mail: sevelean@schaeffler.com Catalog: http://medias.schaeffler.com/medias/en!hp/"

	INCIDENT PROBLEM:
		"-- - 04/11/2012 - 06:53 13-------------- _____________________________________________ From: Sevel, Evan NI/IBC-SIM Sent: Wednesday, April 11, 2012 12:13 AM  Subject: 500 Internal Server Error When trying to access the Self Service to access my benefits and payment, I receive the error “500 Internal Server Error.” Evan P. Sevel International Key Account Manager  Mobile: 704-519-8474 E-mail: sevelean@schaeffler.com Catalog: http://medias.schaeffler.com/medias/en!hp/"

	INCIDENT SOLUTION:
		-- - 04/11/2012 - 06:53 21-------------- SAP HP1 account validity date has been extended.  ---------------------------------------------------


AFTER PREPROCESSOR C:
	2500638,try again. ,"Internal Server Error When trying to access the Self Service to access my benefits and payment, I receive the error Internal Server Error. Evan P. Sevel International Key Account Manager Catalog ",SAP HP account validity date has been extended. 

	REFERENCE NUMBER:
		2500638

	USER INFORMATION:
		try again. 

	INCIDENT PROBLEM:
		"Internal Server Error When trying to access the Self Service to access my benefits and payment, I receive the error Internal Server Error. Evan P. Sevel International Key Account Manager Catalog "

	INCIDENT SOLUTION:
		SAP HP account validity date has been extended. 


PROCESSOR (INTERNAL NLP STEP):
	INCIDENT PROBLEM:
		(S
		  (GPE Intern/NNP)
		  (NP (PERSON Server/NNP Error/NNP) When/NNP)
		  (VP tri/VBG)
		  to/TO
		  (NP access/NN the/DT (ORGANIZATION Self/NNP Servic/NNP))
		  to/TO
		  (NP access/NN)
		  my/PRP$
		  (NP benefit/NNS and/CC payment/NN)
		  ,/,
		  I/PRP
		  (VP receiv/VBP)
		  (NP the/DT error/NN (ORGANIZATION Intern/NNP Server/NNP Error/NNP))
		  ./.)

	INCIDENT SOLUTION:
		(S
		  (NP (ORGANIZATION SAP/NNP) HP/NNP account/NN valid/NN date/NN)
		  ha/VBZ
		  (VP been/VBN extend/VBN)
		  ./.)


AFTER PROCESSOR:
	2500638,"[['server', 'error', 'when'], ['tri'], ['access', 'the', 'self', 'servic'], ['access'], ['benefit', 'and', 'payment'], ['receiv'], ['the', 'error', 'intern', 'server', 'error']]","[['hp', 'account', 'valid', 'date'], ['been', 'extend']]",SAP HP account validity date has been extended. 

	REFERENCE NUMBER:
		2500638

	INCIDENT PROBLEM:
		[
			['server', 'error', 'when'],
			['tri'],
			['access', 'the', 'self', 'servic'],
			['access'],
			['benefit', 'and', 'payment'],
			['receiv'],
			['the', 'error', 'intern', 'server', 'error']
		] 

	INCIDENT SOLUTION:
		[
			['hp', 'account', 'valid', 'date'],
			['been', 'extend']
		] 

	INCIDENT SOLUTION (FROM PREVIOUS STEP):
		SAP HP account validity date has been extended. 