data <- read.csv("survey_861592_R_data_file.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")


# LimeSurvey Field type: F
data[, 1] <- as.numeric(data[, 1])
attributes(data)$variable.labels[1] <- "id"
names(data)[1] <- "id"
# LimeSurvey Field type: DATETIME23.2
data[, 2] <- as.character(data[, 2])
attributes(data)$variable.labels[2] <- "submitdate"
names(data)[2] <- "submitdate"
# LimeSurvey Field type: F
data[, 3] <- as.numeric(data[, 3])
attributes(data)$variable.labels[3] <- "lastpage"
names(data)[3] <- "lastpage"
# LimeSurvey Field type: A
data[, 4] <- as.character(data[, 4])
attributes(data)$variable.labels[4] <- "startlanguage"
names(data)[4] <- "startlanguage"
# LimeSurvey Field type: A
data[, 5] <- as.character(data[, 5])
attributes(data)$variable.labels[5] <- "seed"
names(data)[5] <- "seed"
# LimeSurvey Field type: A
data[, 6] <- as.character(data[, 6])
attributes(data)$variable.labels[6] <- "Tu sei..."
data[, 6] <- factor(data[, 6], levels=c("AO02","AO01","AO03","AO04"),labels=c("Femmina", "Maschio", "Altro", "Preferisco non specificare"))
names(data)[6] <- "sex"
# LimeSurvey Field type: A
data[, 7] <- as.character(data[, 7])
attributes(data)$variable.labels[7] <- "Indica la tua età in anni compiuti"
data[, 7] <- factor(data[, 7], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13","AO14","AO15","AO16","AO17","AO18","AO19","AO20","AO21","AO22","AO23","AO24","AO25","AO26","AO27","AO28","AO29","AO30","AO31","AO32","AO33","AO34","AO35","AO36","AO37","AO38","AO39","AO40","AO41","AO42","AO43","AO44","AO45","AO46","AO47","AO48","AO49","AO50","AO51","AO52","AO53","AO54","AO55","AO56","AO57","AO58","AO59","AO60","AO61","AO62","AO63","AO64","AO65","AO66","AO67","AO68","AO69","AO70","AO71","AO72","AO73","AO74","AO75","AO76","AO77","AO78","AO79","AO80","AO81","AO82","AO83","AO84"),labels=c("18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100", "Più di 100"))
names(data)[7] <- "age"
# LimeSurvey Field type: A
data[, 8] <- as.character(data[, 8])
attributes(data)$variable.labels[8] <- "In quale regione italiana abiti? "
data[, 8] <- factor(data[, 8], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13","AO14","AO15","AO16","AO17","AO18","AO19","AO20","AO21"),labels=c("Abruzzo", "Basilicata", "Calabria", "Campania", "Emilia-Romagna", "Friuli-Venezia Giulia", "Lazio", "Liguria", "Lombardia", "Marche", "Molise", "Piemonte", "Puglia", "Sardegna", "Sicilia", "Toscana", "Trentino-Alto Adige", "Umbria", "Val d\'Aosta", "Veneto", "Non abito in Italia"))
names(data)[8] <- "region"
# LimeSurvey Field type: A
data[, 9] <- as.character(data[, 9])
attributes(data)$variable.labels[9] <- "Quale dei seguenti termini utlizzeresti per descrivere il luogo in cui abiti?"
data[, 9] <- factor(data[, 9], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Una grande città", "I sobborghi o la periferia di una grande città", "Una città di medie dimensioni o cittadina", "Un paese", "Una casa isolata", "Non saprei"))
names(data)[9] <- "citysize"
# LimeSurvey Field type: A
data[, 10] <- as.character(data[, 10])
attributes(data)$variable.labels[10] <- "Qual è il tuo titolo di studio?"
data[, 10] <- factor(data[, 10], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11"),labels=c("Licenza elementare/nessun titolo", "Licenza media inferiore/avviamento", "Diploma qualifica professionale (2-3 anni)", "Diploma maturità istituto professionale (5 anni)", "Diploma maturità istituto tecnico", "Diploma maturità liceo classico o scientifico", "Altro diploma maturità (istituto magistrale, liceo linguistico, liceo artistico, liceo socio-psico-pedagogico/scienze um", "Laurea scientifica (3/4/5 anni, laurea triennale, laurea specialistica) (include medicina, biologia ed economia)", "Laurea umanistica (3/4/5 anni, laurea triennale, laurea specialistica) (include psicologia, sociologia e scienza politic", "Master/Scuola di specializzazione post-laurea", "Dottorato di ricerca"))
names(data)[10] <- "educ"
# LimeSurvey Field type: A
data[, 11] <- as.character(data[, 11])
attributes(data)$variable.labels[11] <- "Quale delle seguenti descrive meglio la tua condizione lavorativa?"
data[, 11] <- factor(data[, 11], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9"),labels=c("Lavoratore/lavoratrice dipendente", "Lavoratore/lavoratrice autonomo/a", "Pensionato/a", "Casalingo/a", "Studente/essa", "Disoccupato/a", "Cassa integrazione guadagni o mobilità", "In cerca di prima occupazione", "Nessuna delle precedenti"))
names(data)[11] <- "job1"
# LimeSurvey Field type: A
data[, 12] <- as.character(data[, 12])
attributes(data)$variable.labels[12] <- "Quale delle seguenti categorie descrive meglio la tua occupazione principale?"
data[, 12] <- factor(data[, 12], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13"),labels=c("Professionista in campo tecnico (ingegnere, architetto, specialista I.T.)", "Professionista in campo legale/affari (avvocato, giudice, notaio, contabile, broker, ...)", "Professionista in campo socio-culturale (artista, personale di teatro/museo, giornalista, etc.)", "Professionista in campo sanitario/socio-assistenziale", "Direttore o top manager di azienda pubblica/privata", "Medio dirigente di azienda pubblica/privata", "Insegnante (scuola o università) o ricercatore", "Dipendente pubblico", "Impiegato d\'ufficio", "Impiegato di altro tipo (addetto/a vendite, infermiere/a, cameriere/a, autista, addetto/a pulizie, impiegato/a in serviz", "Caposquadra/supervisore in lavoro manuale", "Lavoratore manuale", "Nessuna delle precedenti"))
names(data)[12] <- "job2"
# LimeSurvey Field type: A
data[, 13] <- as.character(data[, 13])
attributes(data)$variable.labels[13] <- "Quale delle seguenti categorie descrive meglio la tua occupazione principale?"
data[, 13] <- factor(data[, 13], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09"),labels=c("Agricoltore/Pescatore", "Proprietario di un negozio/Artigiano", "Tecnico o addetto riparazioni", "Professionista in campo tecnico (ingegnere, architetto, specialista I.T.)", "Professionista in campo legale/affari (avvocato, notaio, contabile, broker, ...)", "Professionista in campo socio-culturale (artista, giornalista, autore, etc.)", "Professionista in campo sanitario/socio-assistenziale", "Manager di un\'azienda", "Nessuna delle precedenti"))
names(data)[13] <- "job3"
# LimeSurvey Field type: A
data[, 14] <- as.character(data[, 14])
attributes(data)$variable.labels[14] <- "Quale delle seguenti descrizioni si avvicina maggiormente al modo in cui ti senti, di questi tempi, riguardo al tuo reddito o al reddito della tua famiglia?"
data[, 14] <- factor(data[, 14], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Vivo senza problemi e riesco a mettere da parte dei risparmi", "Vivo senza problemi ma non riesco a mettere da parte dei risparmi", "Riesco ad arrangiarmi facendo qualche rinuncia", "Ho delle difficoltà e devo fare tante rinunce ma riesco a vivere col mio reddito", "Ho molte difficoltà e non riesco a vivere con il mio reddito", "Preferisco non rispondere"))
names(data)[14] <- "income"
# LimeSurvey Field type: A
data[, 15] <- as.character(data[, 15])
attributes(data)$variable.labels[15] <- "[Dove ti collocheresti?] Ci sono persone che tendono ad occupare il vertice della nostra società e persone che tendono ad occupare il fondo. Tu..."
data[, 15] <- factor(data[, 15], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12"),labels=c("0 - Al fondo della nostra società", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Al vertice della nostra società", "Preferisco non rispondere"))
names(data)[15] <- "socposition_SQ001"
# LimeSurvey Field type: A
data[, 16] <- as.character(data[, 16])
attributes(data)$variable.labels[16] <- "Qual è il titolo di studio più alto conseguito dai tuoi genitori?"
data[, 16] <- factor(data[, 16], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12"),labels=c("Licenza elementare/nessun titolo", "Licenza media inferiore/avviamento", "Diploma qualifica professionale (2-3 anni)", "Diploma maturità professionale (compreso istituto d’arte)", "Diploma maturità tecnica", "Diploma maturità liceo classico o scientifico", "Altro diploma maturità (istituto magistrale, liceo linguistico, liceo artistico, liceo socio-psico-pedagogico)", "Laurea Scientifica (3/4/5 anni, laurea triennale, laurea specialistica) (include medicina, biologia ed economia)", "Laurea Umanistica (3/4/5 anni, laurea triennale, laurea specialistica) (include psicologia, sociologia e scienza politic", "Master/Scuola di specializzazione post-laurea", "Dottorato di ricerca"))
names(data)[16] <- "pareduc"
# LimeSurvey Field type: A
data[, 17] <- as.character(data[, 17])
attributes(data)$variable.labels[17] <- "[Sashimi giapponese] Vorremmo sapere qualcosa di più riguardo i tuoi gusti alimentari. In una scala da 1 a 5, dove 1 significa che quel piatto non ti piace affatto e 5 significa che quel piatto ti piace molto, dove collocheresti i seguenti piatti?"
data[, 17] <- factor(data[, 17], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("1 - Non mi piace affatto", "2", "3", "4", "5 - Mi piace molto", "Non lo conosco"))
names(data)[17] <- "taste_sshm"
# LimeSurvey Field type: A
data[, 18] <- as.character(data[, 18])
attributes(data)$variable.labels[18] <- "[Risotto alla milanese] Vorremmo sapere qualcosa di più riguardo i tuoi gusti alimentari. In una scala da 1 a 5, dove 1 significa che quel piatto non ti piace affatto e 5 significa che quel piatto ti piace molto, dove collocheresti i seguenti piatti?"
data[, 18] <- factor(data[, 18], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("1 - Non mi piace affatto", "2", "3", "4", "5 - Mi piace molto", "Non lo conosco"))
names(data)[18] <- "taste_rstt"
# LimeSurvey Field type: A
data[, 19] <- as.character(data[, 19])
attributes(data)$variable.labels[19] <- "[Polpettone] Vorremmo sapere qualcosa di più riguardo i tuoi gusti alimentari. In una scala da 1 a 5, dove 1 significa che quel piatto non ti piace affatto e 5 significa che quel piatto ti piace molto, dove collocheresti i seguenti piatti?"
data[, 19] <- factor(data[, 19], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("1 - Non mi piace affatto", "2", "3", "4", "5 - Mi piace molto", "Non lo conosco"))
names(data)[19] <- "taste_plpt"
# LimeSurvey Field type: A
data[, 20] <- as.character(data[, 20])
attributes(data)$variable.labels[20] <- "[Burger di lenticchie] Vorremmo sapere qualcosa di più riguardo i tuoi gusti alimentari. In una scala da 1 a 5, dove 1 significa che quel piatto non ti piace affatto e 5 significa che quel piatto ti piace molto, dove collocheresti i seguenti piatti?"
data[, 20] <- factor(data[, 20], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("1 - Non mi piace affatto", "2", "3", "4", "5 - Mi piace molto", "Non lo conosco"))
names(data)[20] <- "taste_brgr"
# LimeSurvey Field type: A
data[, 21] <- as.character(data[, 21])
attributes(data)$variable.labels[21] <- "[Pasta alla norma] Vorremmo sapere qualcosa di più riguardo i tuoi gusti alimentari. In una scala da 1 a 5, dove 1 significa che quel piatto non ti piace affatto e 5 significa che quel piatto ti piace molto, dove collocheresti i seguenti piatti?"
data[, 21] <- factor(data[, 21], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("1 - Non mi piace affatto", "2", "3", "4", "5 - Mi piace molto", "Non lo conosco"))
names(data)[21] <- "taste_pstn"
# LimeSurvey Field type: A
data[, 22] <- as.character(data[, 22])
attributes(data)$variable.labels[22] <- "[Curry thailandese] Vorremmo sapere qualcosa di più riguardo i tuoi gusti alimentari. In una scala da 1 a 5, dove 1 significa che quel piatto non ti piace affatto e 5 significa che quel piatto ti piace molto, dove collocheresti i seguenti piatti?"
data[, 22] <- factor(data[, 22], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("1 - Non mi piace affatto", "2", "3", "4", "5 - Mi piace molto", "Non lo conosco"))
names(data)[22] <- "taste_crrt"
# LimeSurvey Field type: A
data[, 23] <- as.character(data[, 23])
attributes(data)$variable.labels[23] <- "[Tartare di manzo] Vorremmo sapere qualcosa di più riguardo i tuoi gusti alimentari. In una scala da 1 a 5, dove 1 significa che quel piatto non ti piace affatto e 5 significa che quel piatto ti piace molto, dove collocheresti i seguenti piatti?"
data[, 23] <- factor(data[, 23], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("1 - Non mi piace affatto", "2", "3", "4", "5 - Mi piace molto", "Non lo conosco"))
names(data)[23] <- "taste_trtr"
# LimeSurvey Field type: A
data[, 24] <- as.character(data[, 24])
attributes(data)$variable.labels[24] <- "[Latte di soia] Vorremmo sapere qualcosa di più riguardo i tuoi gusti alimentari. In una scala da 1 a 5, dove 1 significa che quel piatto non ti piace affatto e 5 significa che quel piatto ti piace molto, dove collocheresti i seguenti piatti?"
data[, 24] <- factor(data[, 24], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("1 - Non mi piace affatto", "2", "3", "4", "5 - Mi piace molto", "Non lo conosco"))
names(data)[24] <- "taste_ltds"
# LimeSurvey Field type: A
data[, 25] <- as.character(data[, 25])
attributes(data)$variable.labels[25] <- "[Andare al cinema] Per ognuna delle seguenti attività, indica quanto spesso le hai svolte nell\'ultimo anno."
data[, 25] <- factor(data[, 25], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Ogni giorno", "Almeno una volta a settimana", "Almeno una volta al mese", "Almeno una volta nell\'ultimo anno", "Mai nell\'ultimo anno", "Non ricordo"))
names(data)[25] <- "cultural_cnma"
# LimeSurvey Field type: A
data[, 26] <- as.character(data[, 26])
attributes(data)$variable.labels[26] <- "[Assistere ad uno spettacolo dal vivo (concerti, opera, balletto, o altri tipi di performance organizzate)] Per ognuna delle seguenti attività, indica quanto spesso le hai svolte nell\'ultimo anno."
data[, 26] <- factor(data[, 26], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Ogni giorno", "Almeno una volta a settimana", "Almeno una volta al mese", "Almeno una volta nell\'ultimo anno", "Mai nell\'ultimo anno", "Non ricordo"))
names(data)[26] <- "cultural_sptt"
# LimeSurvey Field type: A
data[, 27] <- as.character(data[, 27])
attributes(data)$variable.labels[27] <- "[Visitare un sito culturale (monumento, museo, galleria d\'arte, sito archeologico, etc...)] Per ognuna delle seguenti attività, indica quanto spesso le hai svolte nell\'ultimo anno."
data[, 27] <- factor(data[, 27], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Ogni giorno", "Almeno una volta a settimana", "Almeno una volta al mese", "Almeno una volta nell\'ultimo anno", "Mai nell\'ultimo anno", "Non ricordo"))
names(data)[27] <- "cultural_mnmt"
# LimeSurvey Field type: A
data[, 28] <- as.character(data[, 28])
attributes(data)$variable.labels[28] <- "[Praticare un\'attività artistica (suonare uno strumento, cantare, dipingere, disegnare, scrivere poesia o racconti, praticare arti visive, danzare, etc...)] Per ognuna delle seguenti attività, indica quanto spesso le hai svolte nell\'ultimo anno."
data[, 28] <- factor(data[, 28], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Ogni giorno", "Almeno una volta a settimana", "Almeno una volta al mese", "Almeno una volta nell\'ultimo anno", "Mai nell\'ultimo anno", "Non ricordo"))
names(data)[28] <- "cultural_prtc"
# LimeSurvey Field type: A
data[, 29] <- as.character(data[, 29])
attributes(data)$variable.labels[29] <- "[Estroversa, esuberante] Per ognuna delle seguenti affermazioni, indica quanto sei d\'accordo. \"Mi ritengo una persona...\""
data[, 29] <- factor(data[, 29], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente d\'accordo", "Poco d\'accordo", "Né d\'accordo né in disaccordo", "Abbastanza d\'accordo", "Pienamente d\'accordo"))
names(data)[29] <- "bigfive_ext"
# LimeSurvey Field type: A
data[, 30] <- as.character(data[, 30])
attributes(data)$variable.labels[30] <- "[Polemica, litigiosa] Per ognuna delle seguenti affermazioni, indica quanto sei d\'accordo. \"Mi ritengo una persona...\""
data[, 30] <- factor(data[, 30], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente d\'accordo", "Poco d\'accordo", "Né d\'accordo né in disaccordo", "Abbastanza d\'accordo", "Pienamente d\'accordo"))
names(data)[30] <- "bigfive_revagr"
# LimeSurvey Field type: A
data[, 31] <- as.character(data[, 31])
attributes(data)$variable.labels[31] <- "[Affidabile, auto-disciplinata] Per ognuna delle seguenti affermazioni, indica quanto sei d\'accordo. \"Mi ritengo una persona...\""
data[, 31] <- factor(data[, 31], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente d\'accordo", "Poco d\'accordo", "Né d\'accordo né in disaccordo", "Abbastanza d\'accordo", "Pienamente d\'accordo"))
names(data)[31] <- "bigfive_con"
# LimeSurvey Field type: A
data[, 32] <- as.character(data[, 32])
attributes(data)$variable.labels[32] <- "[Ansiosa, che si agita facilmente] Per ognuna delle seguenti affermazioni, indica quanto sei d\'accordo. \"Mi ritengo una persona...\""
data[, 32] <- factor(data[, 32], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente d\'accordo", "Poco d\'accordo", "Né d\'accordo né in disaccordo", "Abbastanza d\'accordo", "Pienamente d\'accordo"))
names(data)[32] <- "bigfive_neu"
# LimeSurvey Field type: A
data[, 33] <- as.character(data[, 33])
attributes(data)$variable.labels[33] <- "[Aperta alle nuove esperienze, con molti interessi] Per ognuna delle seguenti affermazioni, indica quanto sei d\'accordo. \"Mi ritengo una persona...\""
data[, 33] <- factor(data[, 33], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente d\'accordo", "Poco d\'accordo", "Né d\'accordo né in disaccordo", "Abbastanza d\'accordo", "Pienamente d\'accordo"))
names(data)[33] <- "bigfive_ope"
# LimeSurvey Field type: A
data[, 34] <- as.character(data[, 34])
attributes(data)$variable.labels[34] <- "[Riservata, silenziosa] Per ognuna delle seguenti affermazioni, indica quanto sei d\'accordo. \"Mi ritengo una persona...\""
data[, 34] <- factor(data[, 34], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente d\'accordo", "Poco d\'accordo", "Né d\'accordo né in disaccordo", "Abbastanza d\'accordo", "Pienamente d\'accordo"))
names(data)[34] <- "bigfive_revext"
# LimeSurvey Field type: A
data[, 35] <- as.character(data[, 35])
attributes(data)$variable.labels[35] <- "[Comprensiva, affettuosa] Per ognuna delle seguenti affermazioni, indica quanto sei d\'accordo. \"Mi ritengo una persona...\""
data[, 35] <- factor(data[, 35], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente d\'accordo", "Poco d\'accordo", "Né d\'accordo né in disaccordo", "Abbastanza d\'accordo", "Pienamente d\'accordo"))
names(data)[35] <- "bigfive_agr"
# LimeSurvey Field type: A
data[, 36] <- as.character(data[, 36])
attributes(data)$variable.labels[36] <- "[Disorganizzata, distratta] Per ognuna delle seguenti affermazioni, indica quanto sei d\'accordo. \"Mi ritengo una persona...\""
data[, 36] <- factor(data[, 36], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente d\'accordo", "Poco d\'accordo", "Né d\'accordo né in disaccordo", "Abbastanza d\'accordo", "Pienamente d\'accordo"))
names(data)[36] <- "bigfive_revcon"
# LimeSurvey Field type: A
data[, 37] <- as.character(data[, 37])
attributes(data)$variable.labels[37] <- "[Tranquilla, emotivamente stabile] Per ognuna delle seguenti affermazioni, indica quanto sei d\'accordo. \"Mi ritengo una persona...\""
data[, 37] <- factor(data[, 37], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente d\'accordo", "Poco d\'accordo", "Né d\'accordo né in disaccordo", "Abbastanza d\'accordo", "Pienamente d\'accordo"))
names(data)[37] <- "bigfive_revneu"
# LimeSurvey Field type: A
data[, 38] <- as.character(data[, 38])
attributes(data)$variable.labels[38] <- "[Tradizionalista, abitudinaria] Per ognuna delle seguenti affermazioni, indica quanto sei d\'accordo. \"Mi ritengo una persona...\""
data[, 38] <- factor(data[, 38], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente d\'accordo", "Poco d\'accordo", "Né d\'accordo né in disaccordo", "Abbastanza d\'accordo", "Pienamente d\'accordo"))
names(data)[38] <- "bigfive_revope"
# LimeSurvey Field type: A
data[, 39] <- as.character(data[, 39])
attributes(data)$variable.labels[39] <- "In generale, quanto ti interessi di politica?"
data[, 39] <- factor(data[, 39], levels=c("AO01","AO02","AO03","AO04"),labels=c("Molto", "Abbastanza", "Poco", "Per niente"))
names(data)[39] <- "interest"
# LimeSurvey Field type: A
data[, 40] <- as.character(data[, 40])
attributes(data)$variable.labels[40] <- "In una giornata normale, all\'incirca quanto tempo passi a guardare, leggere o ascoltare notizie di politica e attualità? "
data[, 40] <- factor(data[, 40], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Non lo faccio mai", "Mi capita di farlo, ma raramente", "Meno di dieci minuti", "Fra dieci minuti e mezz\'ora", "Fra mezz\'ora e un\'ora", "Fra una e due ore", "Più di due ore"))
names(data)[40] <- "exposure"
# LimeSurvey Field type: F
data[, 41] <- as.numeric(data[, 41])
attributes(data)$variable.labels[41] <- "[Telegiornali (Tg1, Tg2, Tg3, Tg4, Tg5, Studio Aperto, TgLa7...)] Quali fra le seguenti fonti di informazione utilizzi quando ti informi di politica? (Puoi selezionare più di una risposta)"
data[, 41] <- factor(data[, 41], levels=c(1,0),labels=c("Sì", "Non selezionato"))
names(data)[41] <- "infosource_SQ001"
# LimeSurvey Field type: F
data[, 42] <- as.numeric(data[, 42])
attributes(data)$variable.labels[42] <- "[Canali all-news (RaiNews24,  SkyTg24, TgCom24...)] Quali fra le seguenti fonti di informazione utilizzi quando ti informi di politica? (Puoi selezionare più di una risposta)"
data[, 42] <- factor(data[, 42], levels=c(1,0),labels=c("Sì", "Non selezionato"))
names(data)[42] <- "infosource_SQ002"
# LimeSurvey Field type: F
data[, 43] <- as.numeric(data[, 43])
attributes(data)$variable.labels[43] <- "[Giornali tradizionali (Il Corriere, La Repubblica, Il Sole24Ore, Libero, Domani, Il Fatto Quotidiano...)] Quali fra le seguenti fonti di informazione utilizzi quando ti informi di politica? (Puoi selezionare più di una risposta)"
data[, 43] <- factor(data[, 43], levels=c(1,0),labels=c("Sì", "Non selezionato"))
names(data)[43] <- "infosource_SQ003"
# LimeSurvey Field type: F
data[, 44] <- as.numeric(data[, 44])
attributes(data)$variable.labels[44] <- "[Giornali online (HuffingtonPost, Il Post, Fanpage...)] Quali fra le seguenti fonti di informazione utilizzi quando ti informi di politica? (Puoi selezionare più di una risposta)"
data[, 44] <- factor(data[, 44], levels=c(1,0),labels=c("Sì", "Non selezionato"))
names(data)[44] <- "infosource_SQ004"
# LimeSurvey Field type: F
data[, 45] <- as.numeric(data[, 45])
attributes(data)$variable.labels[45] <- "[Blog/Newsletter] Quali fra le seguenti fonti di informazione utilizzi quando ti informi di politica? (Puoi selezionare più di una risposta)"
data[, 45] <- factor(data[, 45], levels=c(1,0),labels=c("Sì", "Non selezionato"))
names(data)[45] <- "infosource_SQ005"
# LimeSurvey Field type: F
data[, 46] <- as.numeric(data[, 46])
attributes(data)$variable.labels[46] <- "[Canali Youtube/Twitch] Quali fra le seguenti fonti di informazione utilizzi quando ti informi di politica? (Puoi selezionare più di una risposta)"
data[, 46] <- factor(data[, 46], levels=c(1,0),labels=c("Sì", "Non selezionato"))
names(data)[46] <- "infosource_SQ006"
# LimeSurvey Field type: F
data[, 47] <- as.numeric(data[, 47])
attributes(data)$variable.labels[47] <- "[Pagine Facebook/Instagram] Quali fra le seguenti fonti di informazione utilizzi quando ti informi di politica? (Puoi selezionare più di una risposta)"
data[, 47] <- factor(data[, 47], levels=c(1,0),labels=c("Sì", "Non selezionato"))
names(data)[47] <- "infosource_SQ007"
# LimeSurvey Field type: F
data[, 48] <- as.numeric(data[, 48])
attributes(data)$variable.labels[48] <- "[Profili TikTok] Quali fra le seguenti fonti di informazione utilizzi quando ti informi di politica? (Puoi selezionare più di una risposta)"
data[, 48] <- factor(data[, 48], levels=c(1,0),labels=c("Sì", "Non selezionato"))
names(data)[48] <- "infosource_SQ008"
# LimeSurvey Field type: F
data[, 49] <- as.numeric(data[, 49])
attributes(data)$variable.labels[49] <- "[Trasmissioni radiofoniche] Quali fra le seguenti fonti di informazione utilizzi quando ti informi di politica? (Puoi selezionare più di una risposta)"
data[, 49] <- factor(data[, 49], levels=c(1,0),labels=c("Sì", "Non selezionato"))
names(data)[49] <- "infosource_SQ009"
# LimeSurvey Field type: F
data[, 50] <- as.numeric(data[, 50])
attributes(data)$variable.labels[50] <- "[Podcast] Quali fra le seguenti fonti di informazione utilizzi quando ti informi di politica? (Puoi selezionare più di una risposta)"
data[, 50] <- factor(data[, 50], levels=c(1,0),labels=c("Sì", "Non selezionato"))
names(data)[50] <- "infosource_SQ010"
# LimeSurvey Field type: F
data[, 51] <- as.numeric(data[, 51])
attributes(data)$variable.labels[51] <- "[Altro] Quali fra le seguenti fonti di informazione utilizzi quando ti informi di politica? (Puoi selezionare più di una risposta)"
data[, 51] <- factor(data[, 51], levels=c(1,0),labels=c("Sì", "Non selezionato"))
names(data)[51] <- "infosource_SQ011"
# LimeSurvey Field type: A
data[, 52] <- as.character(data[, 52])
attributes(data)$variable.labels[52] <- "[Dove ti collocheresti?] Molta gente quando parla di politica usa i termini ‘sinistra’ e ‘destra’. Nella scala riportata sotto, tu..."
data[, 52] <- factor(data[, 52], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13"),labels=c("0 - Estrema sinistra", "1", "2", "3", "4", "5 - Centro", "6", "7", "8", "9", "10 - Estrema destra", "Da nessuna parte"))
names(data)[52] <- "ideology_SQ001"
# LimeSurvey Field type: A
data[, 53] <- as.character(data[, 53])
attributes(data)$variable.labels[53] <- "[Fratelli d\'Italia  ] Vorremmo sapere che cosa pensi di ciascuno dei principali partiti politici italiani. Per ognuno di questi, ti chiediamo di dare un voto su una scala da 0 a 10, dove 0 significa che quel partito non ti piace affatto, e 10 significa che quel partito ti piace molto."
data[, 53] <- factor(data[, 53], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12"),labels=c("0 - Non mi piace affatto", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Mi piace molto", "Non conosco"))
names(data)[53] <- "feelterm_fdi"
# LimeSurvey Field type: A
data[, 54] <- as.character(data[, 54])
attributes(data)$variable.labels[54] <- "[Lega] Vorremmo sapere che cosa pensi di ciascuno dei principali partiti politici italiani. Per ognuno di questi, ti chiediamo di dare un voto su una scala da 0 a 10, dove 0 significa che quel partito non ti piace affatto, e 10 significa che quel partito ti piace molto."
data[, 54] <- factor(data[, 54], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12"),labels=c("0 - Non mi piace affatto", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Mi piace molto", "Non conosco"))
names(data)[54] <- "feelterm_lega"
# LimeSurvey Field type: A
data[, 55] <- as.character(data[, 55])
attributes(data)$variable.labels[55] <- "[Forza Italia  ] Vorremmo sapere che cosa pensi di ciascuno dei principali partiti politici italiani. Per ognuno di questi, ti chiediamo di dare un voto su una scala da 0 a 10, dove 0 significa che quel partito non ti piace affatto, e 10 significa che quel partito ti piace molto."
data[, 55] <- factor(data[, 55], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12"),labels=c("0 - Non mi piace affatto", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Mi piace molto", "Non conosco"))
names(data)[55] <- "feelterm_fi"
# LimeSurvey Field type: A
data[, 56] <- as.character(data[, 56])
attributes(data)$variable.labels[56] <- "[Azione-Italia Viva] Vorremmo sapere che cosa pensi di ciascuno dei principali partiti politici italiani. Per ognuno di questi, ti chiediamo di dare un voto su una scala da 0 a 10, dove 0 significa che quel partito non ti piace affatto, e 10 significa che quel partito ti piace molto."
data[, 56] <- factor(data[, 56], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12"),labels=c("0 - Non mi piace affatto", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Mi piace molto", "Non conosco"))
names(data)[56] <- "feelterm_aziv"
# LimeSurvey Field type: A
data[, 57] <- as.character(data[, 57])
attributes(data)$variable.labels[57] <- "[Movimento 5 Stelle  ] Vorremmo sapere che cosa pensi di ciascuno dei principali partiti politici italiani. Per ognuno di questi, ti chiediamo di dare un voto su una scala da 0 a 10, dove 0 significa che quel partito non ti piace affatto, e 10 significa che quel partito ti piace molto."
data[, 57] <- factor(data[, 57], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12"),labels=c("0 - Non mi piace affatto", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Mi piace molto", "Non conosco"))
names(data)[57] <- "feelterm_m5s"
# LimeSurvey Field type: A
data[, 58] <- as.character(data[, 58])
attributes(data)$variable.labels[58] <- "[Partito Democratico ] Vorremmo sapere che cosa pensi di ciascuno dei principali partiti politici italiani. Per ognuno di questi, ti chiediamo di dare un voto su una scala da 0 a 10, dove 0 significa che quel partito non ti piace affatto, e 10 significa che quel partito ti piace molto."
data[, 58] <- factor(data[, 58], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12"),labels=c("0 - Non mi piace affatto", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Mi piace molto", "Non conosco"))
names(data)[58] <- "feelterm_pd"
# LimeSurvey Field type: A
data[, 59] <- as.character(data[, 59])
attributes(data)$variable.labels[59] <- "[Alleanza Verdi-Sinistra] Vorremmo sapere che cosa pensi di ciascuno dei principali partiti politici italiani. Per ognuno di questi, ti chiediamo di dare un voto su una scala da 0 a 10, dove 0 significa che quel partito non ti piace affatto, e 10 significa che quel partito ti piace molto."
data[, 59] <- factor(data[, 59], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12"),labels=c("0 - Non mi piace affatto", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 - Mi piace molto", "Non conosco"))
names(data)[59] <- "feelterm_avs"
# LimeSurvey Field type: A
data[, 60] <- as.character(data[, 60])
attributes(data)$variable.labels[60] <- "Indica chi fra i seguenti ricopre la carica di PRESIDENTE DELLA REPUBBLICA."
data[, 60] <- factor(data[, 60], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Silvio Berlusconi", "Sergio Mattarella", "Ignazio La Russa", "Giorgia Meloni", "Non saprei"))
names(data)[60] <- "pdr"
# LimeSurvey Field type: A
data[, 61] <- as.character(data[, 61])
attributes(data)$variable.labels[61] <- "Indica chi fra i seguenti ricopre la carica di MINISTRO DELL\'INTERNO."
data[, 61] <- factor(data[, 61], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Matteo Piantedosi", "Carlo Nordio", "Matteo Salvini", "Matteo Renzi", "Non saprei"))
names(data)[61] <- "mdi"
# LimeSurvey Field type: A
data[, 62] <- as.character(data[, 62])
attributes(data)$variable.labels[62] <- "Indica il numero totale di PARLAMENTARI (deputati + senatori) della Repubblica Italiana."
data[, 62] <- factor(data[, 62], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("935", "1000", "200", "600", "Non saprei"))
names(data)[62] <- "nmp"
# LimeSurvey Field type: A
data[, 63] <- as.character(data[, 63])
attributes(data)$variable.labels[63] <- "Pensa ai familiari e parenti che frequenti abitualmente: di quanti di questi ritieni di conoscere l’orientamento politico?"
data[, 63] <- factor(data[, 63], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Nessuno", "Quasi nessuno", "Alcuni", "Circa la metà", "Molti", "Quasi tutti", "Tutti"))
names(data)[63] <- "familyknows"
# LimeSurvey Field type: A
data[, 64] <- as.character(data[, 64])
attributes(data)$variable.labels[64] <- "Quanti tra i tuoi familiari e parenti hanno idee politiche simili alle tue?"
data[, 64] <- factor(data[, 64], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Nessuno", "Quasi nessuno", "Alcuni", "Circa la metà", "Molti", "Quasi tutti", "Tutti"))
names(data)[64] <- "familydisagr"
# LimeSurvey Field type: A
data[, 65] <- as.character(data[, 65])
attributes(data)$variable.labels[65] <- "Pensa ora ai tuoi amici: di quanti di questi ritieni di conoscere l’orientamento politico?"
data[, 65] <- factor(data[, 65], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Nessuno", "Quasi nessuno", "Alcuni", "Circa la metà", "Molti", "Quasi tutti", "Tutti"))
names(data)[65] <- "friendsknows"
# LimeSurvey Field type: A
data[, 66] <- as.character(data[, 66])
attributes(data)$variable.labels[66] <- "Quanti tra i tuoi amici ritieni abbiano idee politiche simili alle tue?"
data[, 66] <- factor(data[, 66], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Nessuno", "Quasi nessuno", "Alcuni", "Circa la metà", "Molti", "Quasi tutti", "Tutti"))
names(data)[66] <- "friendsdisagr"
# LimeSurvey Field type: A
data[, 67] <- as.character(data[, 67])
attributes(data)$variable.labels[67] <- "{if(is_empty(randomnumber), rand(1,5), randomnumber)}"
names(data)[67] <- "randomnumber"
# LimeSurvey Field type: A
data[, 68] <- as.character(data[, 68])
attributes(data)$variable.labels[68] <- "Immagina di essere a pranzo in un ristorante che propone cinque menù al prezzo fisso di 20 euro. Ogni menù comprende un primo, un secondo e un contorno. I cinque menù sono i seguenti:   	Menù base: pasta al sugo, platessa al forno e insalata mista. 	Menù vegano: insalata di farro e quinoa, tofu alla griglia e cavolo nero saltato in padella. 	Menù di carne: tagliere di salumi, salsiccia alla griglia e patate al forno con pancetta croccante. 	Menù tradizionale: trenette al pesto alla ligure, parmigiana di melanzane e cicoria ripassata in padella con fave.  	Menù etnico: noodles in brodo, moussaka e frijoles con guacamole.   Mentre pensi a quale scegliere, senti che la persona seduta nel tavolo accanto al tuo ordina il:   	Menù base: pasta al sugo, platessa al forno e insalata mista."
names(data)[68] <- "controlgroup"
# LimeSurvey Field type: A
data[, 69] <- as.character(data[, 69])
attributes(data)$variable.labels[69] <- "Immagina di essere a pranzo in un ristorante che propone cinque menù al prezzo fisso di 20 euro. Ogni menù comprende un primo, un secondo e un contorno. I cinque menù sono i seguenti:   	Menù base: pasta al sugo, platessa al forno e insalata mista. 	Menù vegano: insalata di farro e quinoa, tofu alla griglia e cavolo nero saltato in padella. 	Menù di carne: tagliere di salumi, salsiccia alla griglia e patate al forno con pancetta croccante. 	Menù tradizionale: trenette al pesto alla ligure, parmigiana di melanzane e cicoria ripassata in padella con fave.  	Menù etnico: noodles in brodo, moussaka e frijoles con guacamole.   Mentre pensi a quale scegliere, senti che la persona seduta nel tavolo accanto al tuo ordina il:   	Menù vegano: insalata di farro e quinoa, tofu alla griglia e cavolo nero saltato in padella."
names(data)[69] <- "treatment1V"
# LimeSurvey Field type: A
data[, 70] <- as.character(data[, 70])
attributes(data)$variable.labels[70] <- "Immagina di essere a pranzo in un ristorante che propone cinque menù al prezzo fisso di 20 euro. Ogni menù comprende un primo, un secondo e un contorno. I cinque menù sono i seguenti:   	Menù base: pasta al sugo, platessa al forno e insalata mista. 	Menù vegano: insalata di farro e quinoa, tofu alla griglia e cavolo nero saltato in padella. 	Menù di carne: tagliere di salumi, salsiccia alla griglia e patate al forno con pancetta croccante. 	Menù tradizionale: trenette al pesto alla ligure, parmigiana di melanzane e cicoria ripassata in padella con fave.  	Menù etnico: noodles in brodo, moussaka e frijoles con guacamole.   Mentre pensi a quale scegliere, senti che la persona seduta nel tavolo accanto al tuo ordina il:   	Menù di carne: tagliere di salumi, salsiccia alla griglia e patate al forno con pancetta croccante."
names(data)[70] <- "treatment2M"
# LimeSurvey Field type: A
data[, 71] <- as.character(data[, 71])
attributes(data)$variable.labels[71] <- "Immagina di essere a pranzo in un ristorante che propone cinque menù al prezzo fisso di 20 euro. Ogni menù comprende un primo, un secondo e un contorno. I cinque menù sono i seguenti:   	Menù base: pasta al sugo, platessa al forno e insalata mista. 	Menù vegano: insalata di farro e quinoa, tofu alla griglia e cavolo nero saltato in padella. 	Menù di carne: tagliere di salumi, salsiccia alla griglia e patate al forno con pancetta croccante. 	Menù tradizionale: trenette al pesto alla ligure, parmigiana di melanzane e cicoria ripassata in padella con fave.  	Menù etnico: noodles in brodo, moussaka e frijoles con guacamole.   Mentre pensi a quale scegliere, senti che la persona seduta nel tavolo accanto al tuo ordina il:   	Menù tradizionale: trenette al pesto alla ligure, parmigiana di melanzane e cicoria ripassata in padella con fave. "
names(data)[71] <- "treatment3T"
# LimeSurvey Field type: A
data[, 72] <- as.character(data[, 72])
attributes(data)$variable.labels[72] <- "Immagina di essere a pranzo in un ristorante che propone cinque menù al prezzo fisso di 20 euro. Ogni menù comprende un primo, un secondo e un contorno. I cinque menù sono i seguenti:   	Menù base: pasta al sugo, platessa al forno e insalata mista. 	Menù vegano: insalata di farro e quinoa, tofu alla griglia e cavolo nero saltato in padella. 	Menù di carne: tagliere di salumi, salsiccia alla griglia e patate al forno con pancetta croccante. 	Menù tradizionale: trenette al pesto alla ligure, parmigiana di melanzane e cicoria ripassata in padella con fave.  	Menù etnico: noodles in brodo, moussaka e frijoles con guacamole.   Mentre pensi a quale scegliere, senti che la persona seduta nel tavolo accanto al tuo ordina il:   	Menù etnico: noodles in brodo, moussaka e frijoles con guacamole."
names(data)[72] <- "treatment4E"
# LimeSurvey Field type: A
data[, 73] <- as.character(data[, 73])
attributes(data)$variable.labels[73] <- "Se dovessi indovinare l’orientamento politico di questa persona, diresti che è di..."
data[, 73] <- factor(data[, 73], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Estrema sinistra", "Sinistra", "Centro-sinistra", "Centro", "Centro-destra", "Destra", "Estrema destra", "Non saprei"))
names(data)[73] <- "expideology"
# LimeSurvey Field type: A
data[, 74] <- as.character(data[, 74])
attributes(data)$variable.labels[74] <- "Inoltre, se dovessi indovinare per quale fra i seguenti partiti vota questa persona, diresti che vota per..."
data[, 74] <- factor(data[, 74], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Alleanza Verdi-Sinistra", "Partito Democratico", "Movimento 5 Stelle", "Azione-Italia Viva", "Forza Italia", "Lega", "Fratelli d’Italia", "Non saprei"))
names(data)[74] <- "expparty"
# LimeSurvey Field type: A
data[, 75] <- as.character(data[, 75])
attributes(data)$variable.labels[75] <- "Cosa ti fa pensare che questa persona voti {INSERTANS:861592X103X1741}?"
names(data)[75] <- "openparty"
# LimeSurvey Field type: A
data[, 76] <- as.character(data[, 76])
attributes(data)$variable.labels[76] <- "Perché pensi di non poter dire nulla sull\'orientamento politico di questa persona?"
data[, 76] <- factor(data[, 76], levels=c("AO01","AO02","AO03","AO04"),labels=c("I suoi gusti sono troppo comuni/generici, potrebbero significare qualsiasi cosa", "I gusti alimentari di una persona non hanno nessun legame con le sue opinioni politiche", "Non mi interessa conoscere le posizioni politiche delle altre persone", "Altro (specificare nel commento)"))
names(data)[76] <- "opennone"
# LimeSurvey Field type: A
data[, 77] <- as.character(data[, 77])
attributes(data)$variable.labels[77] <- "[Commento] Perché pensi di non poter dire nulla sull\'orientamento politico di questa persona?"
names(data)[77] <- "opennone_comment"
# LimeSurvey Field type: A
data[, 78] <- as.character(data[, 78])
attributes(data)$variable.labels[78] <- "Cosa ti fa pensare che questa persona sia di {INSERTANS:861592X103X1740}?"
names(data)[78] <- "openideology"
# LimeSurvey Field type: A
data[, 79] <- as.character(data[, 79])
attributes(data)$variable.labels[79] <- "Se a fine pasto questa persona ti chiedesse di prendere un caffé insieme, accetteresti l\'invito?"
data[, 79] <- factor(data[, 79], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla probabile", "Poco probabile", "Né probabile né improbabile", "Probabile", "Molto probabile"))
names(data)[79] <- "expbehav"
# LimeSurvey Field type: A
data[, 80] <- as.character(data[, 80])
attributes(data)$variable.labels[80] <- "Se questa persona, davanti ad un caffè, volesse parlare di politica e attualità, pensi che potrebbe essere una conversazione piacevole?"
data[, 80] <- factor(data[, 80], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente probabile", "Poco probabile", "Né probabile né improbabile", "Probabile", "Molto probabile"))
names(data)[80] <- "expconv"
# LimeSurvey Field type: A
data[, 81] <- as.character(data[, 81])
attributes(data)$variable.labels[81] <- "Adesso leggerai la descrizione di una persona e alcune sue caratteristiche. Dopodiché, ti sarà chiesto di rispondere ad alcune domande.   Andrea è un grande fan dei film d’autore e gli piace ascoltare la musica indie. Lavora in università come ricercatore. Per andare al lavoro prende la sua bicicletta e ha un gatto di nome Tom."
names(data)[81] <- "andrea"
# LimeSurvey Field type: A
data[, 82] <- as.character(data[, 82])
attributes(data)$variable.labels[82] <- "Se dovessi indovinare l’orientamento politico di Andrea, diresti che è di..."
data[, 82] <- factor(data[, 82], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Estrema sinistra", "Sinistra", "Centro-sinistra", "Centro", "Centro-destra", "Destra", "Estrema destra", "Non saprei"))
names(data)[82] <- "statideology"
# LimeSurvey Field type: A
data[, 83] <- as.character(data[, 83])
attributes(data)$variable.labels[83] <- "Inoltre, se dovessi indovinare per quale fra i seguenti partiti vota Andrea, diresti che vota per..."
data[, 83] <- factor(data[, 83], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Alleanza Verdi-Sinistra", "Partito Democratico", "Movimento 5 Stelle", "Azione-Italia Viva", "Forza Italia", "Lega", "Fratelli d’Italia", "Non saprei"))
names(data)[83] <- "statparty"
# LimeSurvey Field type: A
data[, 84] <- as.character(data[, 84])
attributes(data)$variable.labels[84] <- "Se un giorno Andrea ti invitasse a prendere un caffé insieme, accetteresti l\'invito?"
data[, 84] <- factor(data[, 84], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per nulla probabile", "Poco probabile", "Né probabile né improbabile", "Probabile", "Molto probabile"))
names(data)[84] <- "statbehav"
# LimeSurvey Field type: A
data[, 85] <- as.character(data[, 85])
attributes(data)$variable.labels[85] <- "Se Andrea, davanti ad un caffè, volesse parlare di politica e attualità, pensi che potrebbe essere una conversazione piacevole?"
data[, 85] <- factor(data[, 85], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Per niente probabile", "Poco probabile", "Né probabile né improbabile", "Probabile", "Molto probabile"))
names(data)[85] <- "statconv"


attributes(data)$variable.labels

levels = c(rep("", ncol(data)))
i=1
while(i <=ncol(data))
{
  levels[i] = paste(levels(data[, i]), collapse = "; ")
  i=i+1
}

df=data.frame(names = names(data), labels = attributes(data)$variable.labels, levels = levels)

library(rio)

export(df, "per_federico.xlsx")
