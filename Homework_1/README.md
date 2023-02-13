# README

### Textual Response

**2.2c.** Here we generate two distinct instances `Sym` in the `Structure Sym = SymFCt()` and `LexFct(SymFct())`. They refer to different objects. However, in this case, we want to use the existing structure `Sym` rather than generate an instance. If doing so, in the structure Parser, the TTable and Lex will have two different Symbol table, causing the error. 

**4.** We can contain the substructure that we might need to realize the standalone signature for symbol table. The previous module such as ValSig and SymSig can be reuse in the signature which make explicit in the symbol table signature that any symbol table depends on a Val and a Sym Structure