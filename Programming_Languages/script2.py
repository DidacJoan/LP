#!/usr/bin/python

import urllib
import csv
import sys
import math
import xml.etree.ElementTree as ET

bicisreader = urllib.urlopen('http://wservice.viabicing.cat/getstations.php?v=1')

tree = ET.parse(bicisreader)
root = tree.getroot()

def getSlots(lat,lon):
	solution = []
	for station in root.findall('station'):
		ident = station.find('id').text
		latS = station.find('lat').text
		lonS = station.find('long').text
		slots = station.find('slots').text
		street = station.find('street').text
		dist = distanceOK(float(lat),float(lon),float(latS),float(lonS))
		if float(slots)>0 and dist<1:
			solution.append((ident,dist,street,slots))
	solution.sort(key=lambda x: x[1])
	for station in solution:
		print "id: ",station[0]
		print "distancia: ",station[1]
		print "carrer: ",station[2]
		print "aparcaments lliures: ",station[3],'\n'

def getBikes(lat,lon):
	solution = []
	for station in root.findall('station'):
		ident = station.find('id').text
		latS = station.find('lat').text
		lonS = station.find('long').text
		bikes = station.find('bikes').text
		street = station.find('street').text
		dist = distanceOK(float(lat),float(lon),float(latS),float(lonS))
		if float(bikes)>0 and dist<1:
			solution.append((ident,dist,street,bikes))
	solution.sort(key=lambda x: x[1])
	for station in solution:
		print "id: ",station[0]
		print "distancia: ",station[1]
		print "carrer: ",station[2]
		print "bicicletes: ",station[3],'\n'

def distanceOK(lat1,lon1,lat2,lon2):
	R = 6371
	dLat = deg2rad(lat2-lat1)
	dLon = deg2rad(lon2-lon1)
	a = math.sin(dLat/2)*math.sin(dLat/2)+math.cos(deg2rad(lat1))*math.cos(deg2rad(lat2))*math.sin(dLon/2) * math.sin(dLon/2)
	c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a))
	d = R * c
	return d

def deg2rad(deg):
  return deg * (math.pi/180)

partialname = sys.argv[1]

restaurantfile = open('restaurants.csv','rb')
restaurantreader = csv.reader(restaurantfile)

for line in restaurantreader:
	if partialname in line[0]:
		print "nom: ",line[0]
		print "telefon: ",line[1]
		print "direccio: ",line[2]
		print "barri: ",line[3]
		print "ciutat: ",line[4]
		print "codi postal: ",line[5]
		print "regio: ",line[6]
		print "pais: ",line[7]
		print "web: ",line[10]
		print "mail: ",line[11],'\n'
	
		print "Estacions a menys de 1 Km i aparcament disponible\n"
		getSlots(line[8],line[9])
		print "Estacions a menys de 1 Km i bicicletes disponibles\n"
		getBikes(line[8],line[9])




restaurantfile.close()