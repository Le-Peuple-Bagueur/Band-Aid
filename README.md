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
      Look Ups/tables to merge
      wwww/bird.gif
  
  Step 3 — When you update the app (your normal maintenance loop)
    Whenever you change code:
    
    Edit files locally and upload them again through GitHub (Add file → Upload files), OR
    Use GitHub’s in-browser editor (click a file → pencil icon → commit).
    OR YOU CAN USE THE GITHUB DESKTOP APP - YOU CAN INSTALL IT ON YOUR WORK COMPUTER WITHOUT PERMISSION
    
    BEFORE EDITING: you need to clone the repo locally on your computer, make the changes, do Commit to main, and pull origin. so everything is up to date.


  Step 3.1 create version number before releasing
    To assign version numbers to your application modifications within GitHub, the standard practice involves creating and utilizing Git tags with a semantic versioning (SemVer) format [1]. You         can then manage and release these versions on the GitHub platform. 
    Here methods to do this:
    1. Tag a Specific Commit in Git 
    Git tags are essentially pointers to specific commits, similar to branches, but they are static and do not move. They are ideal for marking release points (v1.0.0, v1.0.1, etc.). 
    Using the Command Line (CLI):
    Navigate to your local repository directory.
    Ensure you are on the correct branch and have the desired changes committed.
    Create an annotated tag (which includes a message and information about the committer, best practice for releases) [1]:
    bash
    git tag -a v1.0.0 -m "Release version 1.0.0"
    Push the tag to your remote repository on GitHub:
    bash
    git push origin v1.0.0
    (Note: You might need to use git push origin --tags to push all new local tags to the remote at once.) [1] 
    Using the GitHub Desktop Application:
    In the GitHub Desktop interface, ensure you are on the commit you wish to tag.
    Go to the History tab.
    Right-click on the desired commit.
    Select Create Tag... and enter your version number (e.g., v1.0.0). 
    2. Create a GitHub Release
    Once a tag is pushed to GitHub, you can formalize it into a GitHub Release, which allows you to add release notes, attach binary files (like compiled app bundles or executables), and provide a      clear, public changelog [1]. 
    On the GitHub Website:
    Navigate to your repository on github.com.
    Click on the Releases tab (usually found in the right sidebar or under the code tab near the file list).
    Click Draft a new release.
    In the "Choose a tag" dropdown, select the tag you just pushed (e.g., v1.0.0).
    Enter a descriptive title and write the release notes (e.g., summary of new features, bug fixes). VERSION NUMBERS CORRESPOND TO THE RELEASE DATE FOR NOW
    (Optional) Attach any application files (APKs, EXEs, etc.) in the "Attach binaries by dropping them here or selecting them" box.
    Click Publish release [1]. 
    


Part B — “Create the ZIP from GitHub” (this is your distribution build step)

  Step 4 — Download the latest repo as a ZIP (no Git required)
  
    Go to your repo page on GitHub
    click on Releases, on the right of the file list.
    choose the Source Code file, it will download de latest version.
    Your browser downloads a ZIP folder in your download folder.


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
