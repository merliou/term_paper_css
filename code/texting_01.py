import googlemaps
import pandas as pd

# API Key
gmaps = googlemaps.Client(key='AIzaSyA34t4OLue4SRaxHQ0SNhoJ-Sfvuvnnblo')

# CSV-Datei mit Orten laden
orte_df = pd.read_csv(r'data\\text_bahnhoefe.csv', encoding='latin1')

# Leere Liste für die Ergebnisse
ergebnisse = []

# Schleife durch jeden Ort in der CSV-Datei
for ort in orte_df['name']:
    # Place ID für den Ort abrufen
    places_result = gmaps.places(ort)
    print(f"Suche nach Ort: {ort}")
    
    # Überprüfen, ob Ergebnisse vorhanden sind
    if places_result['results']:
        place_id = places_result['results'][0]['place_id']
        print(f"Gefundene Place ID: {place_id}")

        # Details des Ortes abrufen, inklusive Rezensionen
        place = gmaps.place(place_id=place_id)

        # Rezensionen durchlaufen und nur 5-Sterne-Rezensionen speichern
        if 'reviews' in place['result']:
            for review in place['result']['reviews']:
                if review['rating'] == 5:
                    ergebnisse.append({
                        'Ort': ort,
                        'place_id': place_id,
                        'Bewertung': review['rating'],
                        'Rezension': review['text']
                    })
                    print(f"Gefundene 5-Sterne-Rezension: {review['text']}")
        else:
            print(f"Keine Rezensionen für Ort: {ort}")
    else:
        print(f"Keine Ergebnisse für Ort: {ort}")

# DataFrame aus den Ergebnissen erstellen
ergebnisse_df = pd.DataFrame(ergebnisse)

# Ergebnisse in eine CSV-Datei speichern
ergebnisse_df.to_csv('5_sterne_rezensionen.csv', index=False)

print("Die 5-Sterne-Rezensionen wurden erfolgreich gespeichert.")