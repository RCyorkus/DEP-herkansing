# Parsers en Ringtones

In dit practicum gaan we aan de slag met audio-synthesis. We gaan audio genereren op basis van een abstracte representatie van noten en instrumenten. Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

Jullie opgaven voor dit practicum staan in de bestanden Parsers.hs en Types.hs; Instruments.hs bevat onze zogenoemde "synthesizermodule". Hier zitten geen opgaven in, maar uiteraard worden de functies daaruit wel gebruikt door de rest van de code.

## `src/Types.hs` (60 punten)

- Schrijf en documenteer zipWithL, die zo ver mogelijk zipt; als na het zippen, nog waarden in de eerste lijst zitten, plakt het deze er achteraan; als na het zippen, nog waarden in de tweede lijst zitten, worden die weggegooid. (10 punten)
- Schrijf en documenteer zipWithR, die zo ver mogelijk zipt; als na het zippen, nog waarden in de eerste lijst zitten, worden die weggegooid; als na het zippen, nog waarden in de tweede lijst zitten, plakt het deze er achteraan. (10 punten)
- Schrijf de instance-declaraties van Semigroup en Monoid voor Sound. Semigroup is een typeclass met een operator (<>), die twee waarden combineert; in deze context betekent dat "twee geluiden na elkaar afspelen". Monoid bouwt voort op Semigroup, maar heeft een mempty; een lege waarde. (10 punten)
- Schrijf en documenteer de operator `(<+>)` die twee `Sound`s  tot een enkel `Sound` combineert. Combineren betekent hier: de geluiden tegelijkertijd afspelen. Als de lijsten niet even lang zijn, moet wat er overblijft achteraan worden toegevoegd! (10 punten)
- Schrijf en documenteer de functie modifyInstrument, die een Modifier met een Instrument combineert. (10 punten)
- Schrijf en documenteer de functie arrange die de functie in het meegegeven Instrument toepast op de frequentie en duur. (10 punten)

## `src/Parsers.hs` (70 punten)

- Schrijf en documenteer de Parser `pComplementCharSet` die een lijst van karakters meekrijgt, en het eerste karakter parset wanneer dit niet in de meegegeven set karakters zit. (10 punten)
- Schrijf en documenteer de Parser `pString` die een gegeven String probeert te parsen. (10 punten)
- Schrijf en documenteer de Parser `pRepeat` die een enkele Parser herhaaldelijk toepast. (10 punten)
- Schrijf en documenteer de Parser `pNumber`, die een geheel getal parset. (10 punten)
- Schrijf en documenteer de Parser `pOctave`, die een getal parset naar de bijbehorende octaaf. (10 punten)
- Schrijf en documenteer de Parser `pHeader`, die de start van de RTTL-string parset. (20 punten)