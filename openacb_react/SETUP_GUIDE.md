# OpenACB Setup Guide
## For Windows users - Super Simple Version

### Step 1: Organize Your Folders

Create this structure (or similar):

```
C:/Juan/Personal/ACB/
├── API_PBP/                    ← Your existing R project
│   ├── openacb.Rproj
│   ├── (all your R scripts)
│   └── OpenACB/                ← Your Shiny app folder
│       ├── ShotChartData25.csv
│       ├── TeamAdvancedStats2025Clean.csv
│       └── (other CSV files)
│
└── OpenACB_React/              ← NEW: Extract the zip here
    ├── dist/                   ← The built app (this is what you view)
    │   ├── index.html
    │   ├── data/               ← JSON data goes here
    │   └── assets/
    ├── src/                    ← Source code (you don't need to touch this)
    └── launch_app.bat          ← Double-click to open!
```

### Step 2: Extract the Zip

1. Download `openacb_react.zip`
2. Right-click → "Extract All..."
3. Extract to `C:/Juan/Personal/ACB/OpenACB_React/`

### Step 3: Launch the App

**Option A - Double-click (easiest):**
- Double-click `launch_app.bat` in the OpenACB_React folder
- Your browser will open automatically

**Option B - From R:**
```r
# Install servr if you don't have it
install.packages("servr")

# Launch the app
servr::httd("C:/Juan/Personal/ACB/OpenACB_React/dist")
```

### Step 4: Update Data (When You Have New Games)

1. Run your normal R pipeline to process new data
2. Run `export_to_react.R` (I provided this file)
3. Refresh your browser

That's it! 🎉

---

## Troubleshooting

**"The page is blank"**
- Make sure you're serving the `dist` folder, not the main folder
- Check that `dist/data/` has JSON files in it

**"I see the old data"**
- Press Ctrl+F5 to hard refresh your browser
- Or clear your browser cache

**"I don't have Python or Node"**
- Just use the R method with `servr::httd()`
- Or use the batch file which tries multiple methods

---

## What Each File Does

| File/Folder | Purpose |
|-------------|---------|
| `dist/` | The compiled app - this is what you actually view |
| `dist/data/` | JSON files that the app reads |
| `src/` | React source code (only needed if you want to modify the app) |
| `package.json` | For developers who want to modify the app |
| `export_to_react.R` | Converts your R data to JSON format |
| `launch_app.bat` | Windows launcher - double-click to open |

