#Downloads are written down where the tests can see them and nowhere else: never
#in the data of whoever runs the tests (see ledgerPath())
options(audioBlastAnalyse.ledger_dir=tempfile("ledger"))
