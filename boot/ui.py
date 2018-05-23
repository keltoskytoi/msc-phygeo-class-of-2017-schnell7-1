####################################################################################################################
#
#
# IMPORT ###
############
import RPi.GPIO as GPIO
import os
import time


#################################################################################################################
#
#
# GPIO SETUP ###
###########
GPIO.setmode(GPIO.BCM)
#GPIO.cleanup()
# LCD
LCD_RS = 17
LCD_E  = 27
LCD_DATA4 = 22
LCD_DATA5 = 10
LCD_DATA6 = 9
LCD_DATA7 = 11

LCD_WIDTH = 16 		# Zeichen je Zeile
LCD_LINE_1 = 0x80 	# Adresse der ersten Display Zeile
LCD_LINE_2 = 0xC0 	# Adresse der zweiten Display Zeile
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
# Schalter
GPIO.setup(16, GPIO.IN, pull_up_down=GPIO.PUD_UP)
GPIO.setup(20, GPIO.IN, pull_up_down=GPIO.PUD_UP)
GPIO.setup(21, GPIO.IN, pull_up_down=GPIO.PUD_UP)
GPIO.setup(12, GPIO.IN, pull_up_down=GPIO.PUD_UP)
##################################################################################################################
#
#
# FUNKTIONEN ###
################
def lcd_send_byte(bits, mode):
	# Pins auf LOW setzen
	GPIO.output(LCD_RS, mode)
	GPIO.output(LCD_DATA4, GPIO.LOW)
	GPIO.output(LCD_DATA5, GPIO.LOW)
	GPIO.output(LCD_DATA6, GPIO.LOW)
	GPIO.output(LCD_DATA7, GPIO.LOW)
	if bits & 0x10 == 0x10:
	  GPIO.output(LCD_DATA4, GPIO.HIGH)
	if bits & 0x20 == 0x20:
	  GPIO.output(LCD_DATA5, GPIO.HIGH)
	if bits & 0x40 == 0x40:
	  GPIO.output(LCD_DATA6, GPIO.HIGH)
	if bits & 0x80 == 0x80:
	  GPIO.output(LCD_DATA7, GPIO.HIGH)
	time.sleep(E_DELAY)    
	GPIO.output(LCD_E, GPIO.HIGH)  
	time.sleep(E_PULSE)
	GPIO.output(LCD_E, GPIO.LOW)  
	time.sleep(E_DELAY)      
	GPIO.output(LCD_DATA4, GPIO.LOW)
	GPIO.output(LCD_DATA5, GPIO.LOW)
	GPIO.output(LCD_DATA6, GPIO.LOW)
	GPIO.output(LCD_DATA7, GPIO.LOW)
	if bits&0x01==0x01:
	  GPIO.output(LCD_DATA4, GPIO.HIGH)
	if bits&0x02==0x02:
	  GPIO.output(LCD_DATA5, GPIO.HIGH)
	if bits&0x04==0x04:
	  GPIO.output(LCD_DATA6, GPIO.HIGH)
	if bits&0x08==0x08:
	  GPIO.output(LCD_DATA7, GPIO.HIGH)
	time.sleep(E_DELAY)    
	GPIO.output(LCD_E, GPIO.HIGH)  
	time.sleep(E_PULSE)
	GPIO.output(LCD_E, GPIO.LOW)  
	time.sleep(E_DELAY)  

def display_init():
	lcd_send_byte(0x33, LCD_CMD)
	lcd_send_byte(0x32, LCD_CMD)
	lcd_send_byte(0x28, LCD_CMD)
	lcd_send_byte(0x0C, LCD_CMD)  
	lcd_send_byte(0x06, LCD_CMD)
	lcd_send_byte(0x01, LCD_CMD)  

def lcd_message(message):
	message = message.ljust(LCD_WIDTH," ")  
	for i in range(LCD_WIDTH):
	  lcd_send_byte(ord(message[i]),LCD_CHR)
	
####################################################################################################################
#
#
# VARS  ###
###########

pos = 0
files = ["sudo python /home/pi/scripte/ultraschall.py", "", ""]
screen = ["ultraschall", "shutdown", "cpu_temp"]
display_init()

#################################################################################################################
#
#
# LOOP ###
##########
while True:
    up = GPIO.input(16)
    yes = GPIO.input(20)
    no = GPIO.input(21)
    down = GPIO.input(12)
    if up == False:
        pos = pos + 1
        print(pos)
        lcd_send_byte(LCD_LINE_1, LCD_CMD)
        lcd_message("> " + screen[pos % len(screen)])
        lcd_send_byte(LCD_LINE_2, LCD_CMD)
        lcd_message("  " +screen[(pos + 1) % len(screen)])
        time.sleep(0.2)
    if no == False:
        print("no")
        print(pos)
        time.sleep(0.2)
    if yes == False:
        if screen[pos % len(screen)] == "ultraschall":
            import ultraschall
            ultraschall.run_ultraschall()
        if screen[pos % len(screen)] == "cpu_temp":
            #os.system('vcgencmd measure_temp')
            first = open("/sys/class/thermal/thermal_zone0/temp", "r")
            temp_cpu = first.readline ()
            digit = float(temp_cpu[:-2])/100
            lcd_send_byte(LCD_LINE_2, LCD_CMD)
            lcd_message("CPU temp: %.2f" % digit)
        if screen[pos %len(screen)] == "shutdown":
            lcd_send_byte(LCD_LINE_2, LCD_CMD)
            lcd_message("Ciao !")
            os.system('sudo shutdown -h 0')
        #print('Button Pressed')
        #print(pos)
        time.sleep(0.2)
