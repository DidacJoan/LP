#!/usr/bin/python

import sys
import csv

from HTMLParser import HTMLParser

allrest=[]

class restaurant: 

    def afegir_nom(self,nom):
        self.nom = nom

    def afegir_ciutat(self,ciutat):
        self.ciutat = ciutat

    def afegir_lat(self,latitud):
        self.latitud = latitud

    def afegir_cod(self,codi):
        self.codi = codi

    def afegir_long(self,longitud):
        self.longitud = longitud

    def afegir_regio(self,regio):
        self.regio = regio

    def afegir_pais(self,pais):
        self.pais = pais

    def afegir_telefon(self,telefon):
    	if hasattr(self,'telefon'):
    		self.telefon.append(telefon)
    	else:
    		self.telefon = []
    		self.telefon.append(telefon)

    def afegir_adress(self,adress):
    	self.adress = adress

    def afegir_url(self,web):
    	self.web = web

    def afegir_mail(self,mail):
    	self.mail = mail

    def afegir_barri(self,barri):
    	self.barri = barri

# creem una subclasse i sobreescribim el metodes del han
class MHTMLParser(HTMLParser):

    crest = restaurant()
    ctag = ""

    def handle_starttag(self, tag, attrs):
        self.ctag = tag
        if tag == 'v:vcard':
            self.crest = restaurant()
        if tag == 'v:url':
            for name, value in attrs:
                if name == 'rdf:resource':
                    self.crest.afegir_url(value)

        if tag == 'rdf:description':
        	for name,value in attrs:
        		if name == 'rdf:about':
        			self.crest.afegir_mail(value)

    def handle_endtag(self, tag):
        self.ctag = ""
        if tag == 'v:vcard':
            allrest.append(self.crest)

    def handle_data(self, data):

        if self.ctag == 'v:fn':
            self.crest.afegir_nom(data)

        if self.ctag == 'v:locality':
            self.crest.afegir_ciutat(data)

        if self.ctag == 'xv:neighborhood':
        	self.crest.afegir_barri(data)

        if self.ctag == 'v:latitude':
            self.crest.afegir_lat(data)

        if self.ctag == 'v:postal-code':
            self.crest.afegir_cod(data)

        if self.ctag == 'v:longitude':
            self.crest.afegir_long(data)

        if self.ctag == 'v:region':
            self.crest.afegir_regio(data)

        if self.ctag == 'v:country-name':
            self.crest.afegir_pais(data)

        if self.ctag == 'rdf:value' and data[0] == '+':
        	self.crest.afegir_telefon(data)

        if self.ctag == 'v:street-address':
        	self.crest.afegir_adress(data)


f = open('restaurants.rdf', 'rb') # obre l'arxiu
rdfSource = f.read()                            
f.close()

parser = MHTMLParser()
parser.feed(rdfSource)
print len(allrest)

restaurantfile = open('restaurants.csv','wb')
restaurantwriter = csv.writer(restaurantfile)

head = ["nom"]+["telefons"]+["direccio"]+["barri"]+["ciutat"]+["codi postal"]+["regio"]+["pais"]+["latitud"]+["longitud"]+["web"]+["mail"]
restaurantwriter.writerows([head])
for r in allrest:
	nom = [r.nom]
	if hasattr(r,'telefon'):
		telf =''
		count = 0
		for it in r.telefon:
			if count == 0:
				count = 1
				telf = telf + it 
			else:
				telf = telf+','+it
	else:
		telf = '-'
	
	if hasattr(r,'adress'):
		adress = [r.adress]
	else:
		adress = ['-']

	if hasattr(r,'barri'):
		barri = [r.barri]
	else:
		barri = ['-']
	
	ciutat = [r.ciutat]

	if hasattr(r,'codi'):
		codi = [r.codi]
	else:
		codi = ['-']
	
	regio = [r.regio]
	pais = [r.pais]
	lat = [r.latitud]
	lon = [r.longitud]
	if hasattr(r,'web'):
		url = [r.web]
	else:
		url = ['-']
	if hasattr(r,'mail'):
		mail = [r.mail]
	else:
		mail = ['-']
	output = nom + [telf] + adress + barri + ciutat + codi + regio + pais + lat + lon + url + mail
	restaurantwriter.writerows([output])
restaurantfile.close()
 