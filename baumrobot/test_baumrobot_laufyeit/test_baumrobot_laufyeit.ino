

void setup() {
  // put your setup code here, to run once:
pinMode(2, INPUT);
pinMode(3, INPUT);
pinMode(5, OUTPUT);
pinMode(6, OUTPUT);
pinMode(7, OUTPUT);
Serial.begin(9600);
}

void loop() {
  // put your main code here, to run repeatedly:
  delay(10);
Serial.print(digitalRead(3));
if (digitalRead(3) == HIGH){
  Serial.print(digitalRead(3));
  if (digitalRead(2) == HIGH){
    analogWrite(5, analogRead(A0));
    digitalWrite(6, HIGH);
    digitalWrite(7, LOW);
  }else{
    analogWrite(5, analogRead(A0));
    digitalWrite(6, LOW);
    digitalWrite(7, HIGH);
  }
}else{
      digitalWrite(6, LOW);
    digitalWrite(7, LOW);
}

}
