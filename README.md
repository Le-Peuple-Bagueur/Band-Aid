INSTRUCTIONS TO MAINTAINER WHEN STARTING FROM SCRATCH

  GO TO PART A3 IF YOU HAVE TO UPDATE SOME R SCRIPT FILES
  
  GO TO PART B AND C TO CREATE A FRESH COPY OF THE APP AND DISTRIBUTE
  
  GO TO PART D FOR INSTRUCTIONS TO USERS

  ***USERS WILL NEED THE GAMEBIRD CSV FILE, AND THE SPECIES LIST FILE, AND THE STATION FILE (IF DESIRED), ON THEIR COMPUTERS***

  

Part A — Maintainer instructions
Step 1 — NO NEED AT THIS POINT: 
    Create the GitHub repository (one-time)
    
    Go to GitHub.com (you can create a GitHub account for yourself as maintainer).
    Create a new repository (public or private).
    
    If you want users to download ZIP without any authentication, public is easiest.
    If your organization requires private code, you can still use private GitHub for you and distribute ZIP via SharePoint (users never see GitHub).


Step 2 — IN CASE YOU COMPLETELY NEED TO START OVER 
    Upload your app files to the repo (no Git)
    You can upload via the GitHub website:
    
    Open your repo page on GitHub
    Click Add file → Upload files
    Drag and drop the whole project contents:
    
    app.R
    your module files (BandAid upload module.R, etc.)
    translations/translation.json
    launcher files (run_app.R, run_app.bat, optional run_app.command)
    
    
    Click Commit changes
    
    
    Tip: Make sure your repo contains the launcher files so you don’t have to “fix the ZIP” after downloading.
    
    Minimum repo content (recommended)
    app.R
    BandAid upload module.R
    BandAid filter module.R
    BandAid Table module.R
    BandAid Plot module.R
    translations/translation.json
    run_app.R
    run_app.bat
    (run_app.command optional)

Step 3 — When you update the app (your normal maintenance loop)
Whenever you change code:

Edit files locally and upload them again through GitHub (Add file → Upload files), OR
Use GitHub’s in-browser editor (click a file → pencil icon → commit).
OR YOU CAN USE THE GITHUB DESKTOP APP - YOU CAN INSTALL IT ON YOUR WORK COMPUTER WITHOUT PERMISSION

BEFORE EDITING: you need to clone the repo locally on your computer, make the changes, do Commit to main, and pull origin. so everything is up to date.


Part B — “Create the ZIP from GitHub” (this is your distribution build step)
Step 4 — Download the latest repo as a ZIP (no Git required)

Go to your repo page on GitHub
Click the green Code button
Click Download ZIP [datanovia.com]
Your browser downloads a ZIP snapshot of the repository files [datanovia.com]

That ZIP is your “build artifact” (your distributable package).
Optional but recommended: rename the ZIP for versions
Before uploading it for users, rename it like:

BandAid_v1.0.zip
BandAid_v1.1_2026-01-29.zip

This makes it much easier to support users (“Which version do you have?”).

Part C — Publish the ZIP for users (Teams / SharePoint / OneDrive)
Step 5 — Upload the ZIP to Teams / SharePoint / OneDrive
Same as before:
Teams

Open Teams → your Team/Channel
Click Files
Upload → select the BandAid_vX.zip
Right-click it → Copy link
Send the link to users

SharePoint

Open your SharePoint document library
Upload the ZIP
Share → copy link

OneDrive

Upload the ZIP to OneDrive
Share → copy link


Part D — User instructions (still no GitHub account)
Users only see the SharePoint/Teams/OneDrive link you send.
Step 6 — What users do

Click your link and download the ZIP
Unzip it:

Windows: right‑click ZIP → Extract All…


Open the extracted folder
Double‑click run_app.bat (Windows)

(or run_app.command on Mac, if you support Macs)


The app opens in their browser and runs locally 
