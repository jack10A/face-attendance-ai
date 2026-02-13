import cv2
import face_recognition
import os
import pandas as pd
from datetime import datetime
import numpy as np
import threading
import warnings
import time
import sys
import pickle
import subprocess
import dlib 
from scipy.spatial import distance as dist 
import speech_recognition as sr  # <--- ADD THIS

warnings.simplefilter(action='ignore', category=FutureWarning)

# --- SETUP ---
path = 'students'
data_path = 'data'
cache_file = 'data/encodings_jitter.pickle'
auth_file = 'auth_token.txt'
landmark_file = 'shape_predictor_68_face_landmarks.dat' 

# Settings
EYE_AR_THRESH = 0.20        
EYE_AR_CONSEC_FRAMES = 2    
MOVEMENT_THRESH = 15        

if not os.path.exists(path): os.makedirs(path)
if not os.path.exists(data_path): os.makedirs(data_path)

# Initialize AI Tools
try:
    detector = dlib.get_frontal_face_detector()
    predictor = dlib.shape_predictor(landmark_file)
    qr_decoder = cv2.QRCodeDetector()
except:
    sys.exit()

def eye_aspect_ratio(eye):
    A = dist.euclidean(eye[1], eye[5])
    B = dist.euclidean(eye[2], eye[4])
    C = dist.euclidean(eye[0], eye[3])
    return (A + B) / (2.0 * C)

def say_text(text):
    def run():
        try:
            safe_text = text.replace("'", "")
            command = f"Add-Type -AssemblyName System.Speech; (New-Object System.Speech.Synthesis.SpeechSynthesizer).Speak('{safe_text}');"
            subprocess.run(["powershell", "-c", command], shell=True)
        except: pass
    threading.Thread(target=run, daemon=True).start()

def load_encodings_robust():
    if not os.path.exists(path): return [], []
    current_files = sorted([f for f in os.listdir(path) if f.endswith(('.jpg', '.jpeg', '.png'))])
    
    if os.path.exists(cache_file):
        try:
            with open(cache_file, 'rb') as f:
                data = pickle.load(f)
            if data['files'] == current_files:
                return data['encodings'], data['names']
        except: pass

    print("ℹ Processing faces...")
    finalEncodings = []; finalNames = []
    
    for i, file in enumerate(current_files):
        img = cv2.imread(f'{path}/{file}')
        if img is None: continue
        img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
        name = os.path.splitext(file)[0]
        try:
            encodes = face_recognition.face_encodings(img, num_jitters=50, model='large')
            if len(encodes) > 0:
                finalEncodings.append(encodes[0])
                finalNames.append(name)
        except: pass

    try:
        with open(cache_file, 'wb') as f:
            pickle.dump({'encodings': finalEncodings, 'names': finalNames, 'files': current_files}, f)
    except: pass
    return finalEncodings, finalNames

encodeListKnown, classNames = load_encodings_robust()

def setup_window():
    cv2.namedWindow('Security Camera', cv2.WINDOW_NORMAL)
    cv2.moveWindow('Security Camera', 50, 50)
    try: cv2.setWindowProperty('Security Camera', cv2.WND_PROP_TOPMOST, 1)
    except: pass

# --- PASTE THIS INTO engine.py (REPLACING THE OLD log_csv FUNCTION) ---

def log_csv(name, course, week, conf):
    safe_course_name = "".join([c if c.isalnum() else "_" for c in course])
    csv_file = f'{data_path}/{safe_course_name}.csv'
    now = datetime.now()
    current_time_str = now.strftime('%H:%M:%S')
    
    # 1. Define Columns (Added DepartureTime)
    cols = ['Name', 'Date', 'ArrivalTime', 'DepartureTime', 'Week', 'Confidence', 'Status']
    
    if not os.path.isfile(csv_file): 
        pd.DataFrame(columns=cols).to_csv(csv_file, index=False)
    
    try:
        attempts = 0; df = pd.DataFrame()
        while attempts < 3:
            try: df = pd.read_csv(csv_file); break
            except: time.sleep(0.05); attempts += 1
        
        if df.empty:
            exists = False
        else:
            # Check if this student exists for this week
            df['Week'] = df['Week'].astype(int)
            mask = (df['Name'] == name) & (df['Week'] == int(week))
            exists = mask.any()
        
        if not exists:
            # --- CHECK-IN (First time seen) ---
            new_data = pd.DataFrame([{
                'Name': name, 
                'Date': now.strftime('%Y-%m-%d'), 
                'ArrivalTime': current_time_str,
                'DepartureTime': current_time_str, # Default to same time initially
                'Week': week, 
                'Confidence': conf, 
                'Status': 'Present'
            }])
            df = pd.concat([df, new_data], ignore_index=True)
            df.to_csv(csv_file, index=False)
            return True # Return True to say "Welcome"
            
        else:
            # --- CHECK-OUT (Seen again) ---
            # Only update if at least 1 minute has passed to avoid double-speaking immediately
            idx = df.index[mask][0]
            existing_arrival = df.at[idx, 'ArrivalTime']
            
            # Simple check: Only update departure if time is different
            if existing_arrival != current_time_str:
                df.at[idx, 'DepartureTime'] = current_time_str
                df.to_csv(csv_file, index=False)
                # We return False here so the voice doesn't say "Welcome" every single frame
                # But you could make it say "Goodbye" if you wanted.
                return False 
                
    except Exception as e: 
        print(f"CSV Error: {e}")
        pass
    return False

def authenticate_doctor():
    print("STARTING_AUTH")
    cap = cv2.VideoCapture(0, cv2.CAP_DSHOW)
    if not cap.isOpened(): cap = cv2.VideoCapture(1)
    setup_window(); start_time = time.time()
    
    while True:
        success, img = cap.read()
        if not success: break
        
        imgS = cv2.resize(img, (0,0), fx=0.25, fy=0.25)
        imgS = cv2.cvtColor(imgS, cv2.COLOR_BGR2RGB)
        faces = face_recognition.face_locations(imgS)
        encodes = face_recognition.face_encodings(imgS, faces)
        
        for encodeFace, faceLoc in zip(encodes, faces):
            matches = face_recognition.compare_faces(encodeListKnown, encodeFace)
            faceDis = face_recognition.face_distance(encodeListKnown, encodeFace)
            matchIndex = np.argmin(faceDis)
            if matches[matchIndex]:
                name = classNames[matchIndex].upper()
                if "DOCTOR" in name or "JACK" in name: 
                    with open(auth_file, "w") as f: f.write("SUCCESS")
                    say_text("Access Granted")
                    time.sleep(1.5); cap.release(); cv2.destroyAllWindows(); return
            
            y1, x2, y2, x1 = faceLoc; y1*=4; x2*=4; y2*=4; x1*=4
            cv2.rectangle(img, (x1, y1), (x2, y2), (0, 255, 255), 2)
            cv2.putText(img, "Scanning...", (x1, y2-10), cv2.FONT_HERSHEY_DUPLEX, 0.6, (255,255,255), 1)

        cv2.imshow('Security Camera', img)
        if cv2.waitKey(1) & 0xFF == ord('q'): break
        if time.time() - start_time > 15: break
    cap.release(); cv2.destroyAllWindows()


# --- NEW: BACKGROUND VOICE LISTENER ---
class VoiceListener:
    def __init__(self):
        self.should_close = False
        self.is_listening = False
        self.thread = None
        
    def listen_loop(self):
        """Continuously listens for 'close camera' command"""
        r = sr.Recognizer()
        r.energy_threshold = 3000
        mic = sr.Microphone()
        
        print("🎤 Voice listener started. Say 'Close Camera' to stop.")
        
        with mic as source:
            r.adjust_for_ambient_noise(source, duration=0.5)
            
            while self.is_listening:
                try:
                    audio = r.listen(source, timeout=2, phrase_time_limit=3)
                    command = r.recognize_google(audio).lower()
                    print(f"🎤 Heard: {command}")
                    
                    if "close" in command or "stop" in command or "exit" in command:
                        say_text("Closing Camera")
                        self.should_close = True
                        break
                        
                except sr.WaitTimeoutError:
                    continue
                except sr.UnknownValueError:
                    continue
                except Exception as e:
                    print(f"Voice error: {e}")
                    continue
    
    def start(self):
        """Start listening in background thread"""
        self.is_listening = True
        self.should_close = False
        self.thread = threading.Thread(target=self.listen_loop, daemon=True)
        self.thread.start()
    
    def stop(self):
        """Stop the listener"""
        self.is_listening = False
        if self.thread:
            self.thread.join(timeout=1)


# --- ATTENDANCE MODE (FACE + QR + VOICE) ---
def start_camera(course_in, week_in):
    print(f"LAUNCHING SECURITY CAMERA: {course_in}")
    cap = cv2.VideoCapture(0, cv2.CAP_DSHOW)
    if not cap.isOpened(): cap = cv2.VideoCapture(1)
    setup_window()

    # Start voice listener
    voice_listener = VoiceListener()
    voice_listener.start()

    greeted = []
    blink_counter = 0; is_live = False; last_blink_time = 0
    (lStart, lEnd) = (42, 48); (rStart, rEnd) = (36, 42)

    while True:
        success, img = cap.read()
        if not success: break
        
        # --- CHECK VOICE COMMAND ---
        if voice_listener.should_close:
            print("Voice command received: Closing camera")
            break
        
        # --- 1. CHECK QR CODE FIRST (FASTEST) ---
        data, bbox, _ = qr_decoder.detectAndDecode(img)
        if data:
            match_found = False
            for known_name in classNames:
                if data.upper() in known_name.upper() or known_name.upper() in data.upper():
                    log_csv(known_name.upper(), course_in, week_in, 100)
                    
                    if known_name not in greeted:
                        say_text(f"QR Verified. Welcome {known_name}")
                        greeted.append(known_name)
                    
                    if bbox is not None:
                        for i in range(len(bbox)):
                            pt1 = (int(bbox[i][0][0]), int(bbox[i][0][1]))
                            pt2 = (int(bbox[(i+1) % len(bbox)][0][0]), int(bbox[(i+1) % len(bbox)][0][1]))
                            cv2.line(img, pt1, pt2, (0, 255, 0), 3)
                    cv2.putText(img, f"QR: {known_name}", (50, 50), cv2.FONT_HERSHEY_SIMPLEX, 1, (0, 255, 0), 2)
                    match_found = True
                    break
            
            if not match_found:
                cv2.putText(img, "INVALID QR", (50, 50), cv2.FONT_HERSHEY_SIMPLEX, 1, (0, 0, 255), 2)

        # --- 2. FACE DETECTION ---
        gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
        rects = detector(gray, 0)
        
        for rect in rects:
            shape = predictor(gray, rect)
            shape = np.array([[p.x, p.y] for p in shape.parts()])
            leftEAR = eye_aspect_ratio(shape[lStart:lEnd])
            rightEAR = eye_aspect_ratio(shape[rStart:rEnd])
            avgEAR = (leftEAR + rightEAR) / 2.0
            
            if avgEAR < EYE_AR_THRESH: blink_counter += 1
            else:
                if blink_counter >= EYE_AR_CONSEC_FRAMES:
                    if not is_live: say_text("Verified")
                    is_live = True; last_blink_time = time.time()
                blink_counter = 0
            
            if time.time() - last_blink_time > 5: is_live = False

            x1, y1, x2, y2 = rect.left(), rect.top(), rect.right(), rect.bottom()
            
            if not is_live:
                cv2.rectangle(img, (x1, y1), (x2, y2), (255, 0, 0), 2)
                cv2.putText(img, "BLINK TO VERIFY", (x1, y1 - 10), cv2.FONT_HERSHEY_SIMPLEX, 0.6, (255, 0, 0), 2)
            else:
                face_loc = [(y1, x2, y2, x1)]
                imgRGB = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
                encodes = face_recognition.face_encodings(imgRGB, face_loc)
                
                if len(encodes) > 0:
                    matches = face_recognition.compare_faces(encodeListKnown, encodes[0])
                    faceDis = face_recognition.face_distance(encodeListKnown, encodes[0])
                    matchIndex = np.argmin(faceDis)
                    score = round((1 - faceDis[matchIndex]) * 100)
                    
                    if matches[matchIndex] and faceDis[matchIndex] < 0.45:
                        name = classNames[matchIndex].upper()
                        log_csv(name, course_in, week_in, score)
                        if name not in greeted:
                            say_text(f"Welcome {name}")
                            greeted.append(name)
                        
                        cv2.rectangle(img, (x1, y1), (x2, y2), (0, 255, 0), 2)
                        cv2.putText(img, f"{name} {score}%", (x1, y1 - 10), cv2.FONT_HERSHEY_SIMPLEX, 0.7, (0, 255, 0), 2)
                    else:
                        cv2.rectangle(img, (x1, y1), (x2, y2), (0, 0, 255), 2)
                        cv2.putText(img, "UNKNOWN", (x1, y1 - 10), cv2.FONT_HERSHEY_SIMPLEX, 0.7, (0, 0, 255), 2)

        # Show voice indicator
        cv2.putText(img, "🎤 Say 'Close Camera' to stop", (10, img.shape[0] - 10), 
                    cv2.FONT_HERSHEY_SIMPLEX, 0.6, (255, 255, 0), 2)
        cv2.putText(img, f"Class: {course_in} (Week {week_in})", (10, 30), 
                    cv2.FONT_HERSHEY_SIMPLEX, 0.7, (255, 200, 0), 2)
        
        cv2.imshow('Security Camera', img)
        
        # Check for 'q' key
        if cv2.waitKey(1) & 0xFF == ord('q'): 
            break
                
    # Cleanup
    voice_listener.stop()
    cap.release()
    cv2.destroyAllWindows()
    print("Camera closed successfully")


if __name__ == "__main__":
    if len(sys.argv) > 1:
        if sys.argv[1] == "auth": 
            authenticate_doctor()
        elif len(sys.argv) > 2: 
            start_camera(sys.argv[1], sys.argv[2])
    else: 
        start_camera("Test", "1")
