#Bibliotheken einbinden
import RPi.GPIO as GPIO
import time
import lcd_func
#import csv
#import cgi
#import cgitb
#cgitb.enable()

#GPIO Modus (BOARD / BCM)
GPIO.setmode(GPIO.BCM)


LCD_RS = 17
LCD_E  = 27
LCD_DATA4 = 22
LCD_DATA5 = 10
LCD_DATA6 = 9
LCD_DATA7 = 11

LCD_WIDTH = 16          # Zeichen je Zeile
LCD_LINE_1 = 0x80       # Adresse der ersten Display Zeile
LCD_LINE_2 = 0xC0       # Adresse der zweiten Display Zeile
LCD_CHR = GPIO.HIGH
LCD_CMD = GPIO.LOW
E_PULSE = 0.0005
E_DELAY = 0.0005
GPIO.setup(LCD_E, GPIO.OUT)
GPIO.setup(LCD_RS, GPIO.OUT)
GPIO.setup(LCD_DATA4, GPIO.OUT)
GPIO.setup(LCD_DATA5, GPIO.OUT)
GPIO.setup(LCD_DATA6, GPIO.OUT)
GPIO.setup(LCD_DATA7, GPIO.OUT)
#GPIO Pins zuweisen
GPIO_TRIGGER = 18
GPIO_ECHO = 24
GPIO_EXIT = 21
#Richtung der GPIO-Pins festlegen (IN / OUT)
GPIO.setup(GPIO_TRIGGER, GPIO.OUT)
GPIO.setup(GPIO_ECHO, GPIO.IN)
GPIO.setup(GPIO_EXIT, GPIO.IN, pull_up_down=GPIO.PUD_UP)
def distanz():
    # setze Trigger auf HIGH
    GPIO.output(GPIO_TRIGGER, True)

    # setze Trigger nach 0.01ms aus LOW
    time.sleep(0.00001)
    GPIO.output(GPIO_TRIGGER, False)

    StartZeit = time.time()
    StopZeit = time.time()

    # speichere Startzeit
    while GPIO.input(GPIO_ECHO) == 0:
        StartZeit = time.time()

    # speichere Ankunftszeit
    while GPIO.input(GPIO_ECHO) == 1:
        StopZeit = time.time()

    # Zeit Differenz zwischen Start und Ankunft
    TimeElapsed = StopZeit - StartZeit
    # mit der Schallgeschwindigkeit (34300 cm/s) multiplizieren
    # und durch 2 teilen, da hin und zurueck
    distanz = (TimeElapsed * 34300) / 2

    return distanz

def run_ultraschall():
    a = []
    try:
        while GPIO.input(21):
            abstand = distanz()
            a.append([abstand])
            #print 'Content-type: text/html\n\n'
            lcd_func.lcd_send_byte(LCD_LINE_2, LCD_CMD)
            lcd_func.lcd_message("dist:  %.1f cm" % abstand)
            time.sleep(1)

        # Beim Abbruch durch STRG+C resetten
        lcd_func.lcd_send_byte(LCD_LINE_2, LCD_CMD)
        lcd_func.lcd_message("gestoppt !")
    except KeyboardInterrupt:
        # Write CSV file
        #with open('/home/pi/tab/werte.csv', 'w') as fp:
            #writer = csv.writer(fp, delimiter=',')
            # writer.writerow(["your", "header", "foo"])  # write header
            #writer.writerows(a)
        print("Messung vom User gestoppt")
        GPIO.cleanup()
