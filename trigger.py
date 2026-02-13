import speech_recognition as sr
import sys
import subprocess
import threading

# Feedback function (Text-to-Speech)
def say_text(text):
    def run():
        try:
            safe_text = text.replace("'", "")
            command = f"Add-Type -AssemblyName System.Speech; (New-Object System.Speech.Synthesis.SpeechSynthesizer).Speak('{safe_text}');"
            subprocess.run(["powershell", "-c", command], shell=True)
        except: pass
    t = threading.Thread(target=run, daemon=True)
    t.start()
    t.join() # Wait for speech to finish before closing

def listen_for_trigger():
    r = sr.Recognizer()
    r.energy_threshold = 4000
    
    with sr.Microphone() as source:
        print("Listening for 'Open Camera' or 'Close Camera'...")
        # Adjust for ambient noise briefly
        r.adjust_for_ambient_noise(source, duration=0.5)
        
        try:
            # Listen for 4 seconds maximum
            audio = r.listen(source, timeout=4, phrase_time_limit=3)
            command = r.recognize_google(audio).lower()
            print(f"Heard: {command}")
            
            # Check for OPEN commands
            if "open" in command or "start" in command or "launch" in command:
                say_text("Opening Camera")
                sys.exit(0) # 0 = Open Camera
            
            # Check for CLOSE commands
            elif "close" in command or "stop" in command or "exit" in command or "quit" in command:
                say_text("Closing Camera")
                sys.exit(2) # 2 = Close Camera
            
            else:
                say_text("Command not recognized")
                sys.exit(1) # 1 = Failure
                
        except sr.WaitTimeoutError:
            print("No speech detected.")
            sys.exit(1)
        except Exception as e:
            print(f"Error: {e}")
            sys.exit(1)

if __name__ == "__main__":
    listen_for_trigger()
