Face Attendance AI

Real‑time AI attendance system using facial recognition, liveness detection, QR scanning & voice commands — with an R Shiny dashboard for classroom management.

🚀 Overview

Face Attendance AI is a hybrid AI project combining Python and R to build a complete attendance solution that uses:

📸 Facial Recognition (to mark attendance)

🔐 Liveness Detection (to prevent fake / photo spoofing)

📱 QR Code Scanning & Voice Commands

📊 R Shiny Dashboard (for visualizing, managing and reporting classroom attendance)

This system is ideal for classrooms or small organizations wanting automated attendance with a user‑friendly dashboard.
📁 Project Structure
face‑attendance‑ai/
├── app.R                 # Main R Shiny dashboard
├── engine.py             # Python engine for recognition & attendance logic
├── student_app.R         # R module for student interface
├── student_db.csv        # Sample database of student records
├── test.Rmd              # Testing / documentation notebook
└── trigger.py            # Python trigger script for workflow
🧠 Key Features

✔️ Real‑time facial recognition for marking attendance
✔️ Liveness detection to reduce fraud
✔️ QR code attendance support
✔️ Voice command integration
✔️ R Shiny dashboard for classroom attendance visualization
✔️ Python engine for core AI processing

🛠️ Tech Stack
Component	Technology
Backend / AI Logic	Python
Face Recognition / CV	Python libraries (e.g., OpenCV)
Dashboard & UI	R, Shiny
Database	CSV (can be extended to SQL)
