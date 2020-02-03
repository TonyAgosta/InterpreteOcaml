Il progetto prevede la progettazione e realizzazione di una semplice estensione del linguaggio didattico
funzionale che permetta di manipolare dizionari.

Un dizionario è una collezione di valori identificati univocamente da una chiave: un dizionario è una collezione di coppie chiave-valore dove la chiave è unica.

I dizionari sono caratterizzati da diverse operazioni primitive.

L’operazione insert inserisce una coppia chiave-valore in un dizionario.

L’operazione delete rimuove una coppia chiave-valore da un dizionario.

L’operazione has_key controlla l’esistenza della chiave in un dizionario.

L’operazione Iterate(f,d) applica la funzione f a tutte le coppie chiave-valore presenti nel dizionario, restituendo un nuovo dizionario con i valori ottenuti come risultato della funzione

L’operazione fold(f, d) calcola il valore ottenuto applicando la funzione sequenzialmente a tutti gli elementi del dizionario.
Quindi, sia d il dizionario {k1:v1, k2:v2,..., kn:vn} a valori interi (vi sono interi), sia f una funzione della forma fun acc param -> body, allora fold(f, d) produce come risultato f(f(... f(f(0,v1),v2) ...),vn)
In generale il valore iniziale dell’accumulatore sarà il valore di default dei valori presenti nel dizionario.

L’operazione filter(key list, d) restituisce come risultato il dizionario ottenuto dal dizionario d eliminando tutte le coppie chiave-valore per cui la chiave non appartiene alla lista delle chiavi passata come parametro.
