/**
 * Data Processing Script for OpenACB
 * Converts raw PBP CSV files to processed JSON for the React frontend
 */

import fs from 'fs';
import path from 'path';
import { parse } from 'csv-parse/sync';

const RAW_DIR = '../openacb_api/data/processed';
const OUTPUT_DIR = './public/data';

// Ensure output directory exists
if (!fs.existsSync(OUTPUT_DIR)) {
  fs.mkdirSync(OUTPUT_DIR, { recursive: true });
}

// Read all ShotChartData CSV files for all seasons
function loadAllGames() {
  const seasons = [2021, 2022, 2023, 2024, 2025, 2026];
  let allRecords = [];
  
  seasons.forEach(season => {
    const shotChartFile = path.join(RAW_DIR, `ShotChartData${season}.csv`);
    
    if (!fs.existsSync(shotChartFile)) {
      console.warn(`ShotChartData file not found: ${shotChartFile}`);
      return;
    }
    
    const content = fs.readFileSync(shotChartFile, 'utf-8');
    const records = parse(content, { columns: true, skip_empty_lines: true });
    
    // Add season information to each record
    const recordsWithSeason = records.map(record => ({
      ...record,
      season: season
    }));
    
    allRecords = allRecords.concat(recordsWithSeason);
    console.log(`Loaded ${records.length} shot events from ${season}-${String(season).slice(-2)} season`);
  });
  
  console.log(`Total: ${allRecords.length} shot events from all seasons`);
  return allRecords;
}

// Process shot chart data
function processShotCharts(data) {
  const shotTypes = [
    'Canasta de 3', 'Intento fallado de 3',
    'Canasta de 2', 'Intento fallado de 2',
    'Mate', 'Mate fuera'
  ];
  
  const shots = data.filter(row => shotTypes.includes(row['type.description']));
  
  return shots.map(shot => {
    // Use the pre-calculated rescaled coordinates from CSV
    const x = parseFloat(shot.posX_res) || 0;
    const y = parseFloat(shot.posY_res) || 0;
    
    // Use the pre-calculated distance and angle from CSV
    const distance = parseFloat(shot.distance) || 0;
    
    // Determine if made
    const madeTypes = ['Canasta de 2', 'Canasta de 3', 'Mate'];
    const made = madeTypes.includes(shot['type.description']);
    
    // Determine points
    const isThree = shot['type.description'].includes('3');
    const points = made ? (isThree ? 3 : 2) : 0;
    
    // Use the pre-calculated zone fields from the CSV data
    const zone = shot.zone || 'Other';
    const zoned = shot.zoned || zone;
    
    return {
      id: shot[''] || Math.random().toString(36).substr(2, 9),
      matchId: shot.id_match,
      player: shot['license.licenseNick'] || 'Unknown',
      playerFull: shot['license.licenseStr15'] || 'Unknown',
      team: shot['team.team_actual_name'] || shot.team,
      opponent: shot.opponent,
      period: parseInt(shot.period) || 1,
      minute: parseInt(shot.minute) || 0,
      x,
      y,
      distance: Math.round(distance * 100) / 100,
      made,
      points,
      zone,
      zoned,  // Use the detailed zone field
      shotType: shot['type.description'],
      season: shot.season
    };
  });
}

// Process team statistics from TeamAdvancedStats CSV files
function processTeamStats() {
  const seasons = [2021, 2022, 2023, 2024, 2025, 2026];
  let allTeams = [];
  
  seasons.forEach(season => {
    const teamStatsFile = path.join(RAW_DIR, `TeamAdvancedStats${season}.csv`);
    
    if (!fs.existsSync(teamStatsFile)) {
      console.warn(`TeamAdvancedStats file not found: ${teamStatsFile}`);
      return;
    }
    
    const content = fs.readFileSync(teamStatsFile, 'utf-8');
    const records = parse(content, { columns: true, skip_empty_lines: true });
    
    // Process each team record
    records.forEach(record => {
      const teamName = record['team.team_actual_name'];
      const year = parseInt(record['year']);
      
      // Calculate advanced stats from the available fields
      const teamData = {
        team: teamName,
        season: year,
        games: parseInt(record['ngames']) || 0,
        
        // Offensive stats
        ortg: parseFloat(record['oer']) || 0,
        der: parseFloat(record['der']) || 0,
        efg: parseFloat(record['efg']) || 0,
        ts: parseFloat(record['ts']) || 0,
        threePct: parseFloat(record['threefg']) || 0,
        threeRate: parseFloat(record['threeatt_rate']) || 0,
        astRate: parseFloat(record['S_assist']) || 0,
        tovRate: parseFloat(record['S_Tov']) || 0,
        orbPct: parseFloat(record['S_OffReb']) || 0,
        
        // Defensive stats
        drtg: parseFloat(record['oer_opponent']) || 0,
        opp_efg: parseFloat(record['efg_opponent']) || 0,
        stlRate: parseFloat(record['S_steal_opponent']) || 0,
        blkRate: parseFloat(record['S_blocks_opponent']) || 0,
        drbPct: parseFloat(record['S_DefReb']) || 0,
        
        // Net rating
        netRtg: parseFloat(record['oer']) - parseFloat(record['oer_opponent']),
        
        // Additional stats
        ppg: parseFloat(record['tci']) / (parseInt(record['ngames']) || 1),
        threePct: parseFloat(record['threefg']) * 100,
        
        // Raw counts for reference
        raw: {
          points: parseInt(record['tci']) || 0,
          fgm: parseInt(record['tci']) - parseInt(record['threefg_opponent']) || 0,
          fga: parseInt(record['tci']) || 0,
          tpm: parseInt(record['threefg']) || 0,
          tpa: parseInt(record['threeatt_rate']) || 0,
          ftm: 0,
          fta: 0,
          orb: parseInt(record['S_OffReb']) || 0,
          drb: parseInt(record['S_DefReb']) || 0,
          ast: parseInt(record['S_assist']) || 0,
          stl: parseInt(record['S_steal_opponent']) || 0,
          blk: parseInt(record['S_blocks_opponent']) || 0,
          tov: parseInt(record['S_Tov']) || 0
        }
      };
      
      allTeams.push(teamData);
    });
    
    console.log(`Processed ${records.length} teams from ${season}-${String(season).slice(-2)} season`);
  });
  
  console.log(`Total: ${allTeams.length} team records from all seasons`);
  return allTeams;
}

// Process player statistics from PlayerStats CSV file
function processPlayerStats() {
  const playerStatsFile = path.join(RAW_DIR, 'PlayerStats.csv');
  
  if (!fs.existsSync(playerStatsFile)) {
    console.error(`PlayerStats file not found: ${playerStatsFile}`);
    return [];
  }
  
  const content = fs.readFileSync(playerStatsFile, 'utf-8');
  const records = parse(content, { columns: true, skip_empty_lines: true });
  
  // Process each player record - this is aggregated data, so we'll add season 2025 as default
  // In a production environment, you would want to generate individual season files
  const players = records.map(record => {
    return {
      player: record.player || 'Unknown',
      playerFull: record.player || 'Unknown',
      team: 'All Teams', // This is aggregated data
      season: 2025, // Default season for aggregated data
      games: 1, // Placeholder
      
      // Basic stats
      ppg: parseFloat(record.ppp) || 0,
      rpg: 0, // Not available in aggregated data
      apg: 0, // Not available in aggregated data
      spg: 0, // Not available in aggregated data
      bpg: 0, // Not available in aggregated data
      topg: 0, // Not available in aggregated data
      
      // Advanced stats
      fgPct: 0, // Not available in aggregated data
      threePct: parseFloat(record.fgthree) * 100 || 0,
      ftPct: 0, // Not available in aggregated data
      efg: parseFloat(record.efg) || 0,
      ts: parseFloat(record.ts) || 0,
      
      // Percentile stats (from the aggregated data)
      ppgPct: parseFloat(record.ppp_pct) || 0,
      efgPct: parseFloat(record.efg_pct) || 0,
      ortgPct: parseFloat(record.ORtg_pct) || 0,
      threePctPct: parseFloat(record.fgthree_pct) || 0,
      usgPct: parseFloat(record.Usg_pct) || 0,
      astRatePct: parseFloat(record.S_assist_pct) || 0,
      tovRatePct: parseFloat(record.S_Tov_pct) || 0,
      threeRatePct: parseFloat(record.X3pt_attrate_pct) || 0,
      rebRatePct: parseFloat(record.S_Reb_pct) || 0,
      stlRatePct: parseFloat(record.S_steal_pct) || 0,
      blkRatePct: parseFloat(record.S_block_pct) || 0,
      
      // Raw stats (limited)
      raw: {
        points: parseFloat(record.ppp) || 0,
        fgm: 0,
        fga: 0,
        tpm: parseFloat(record.fgthree) || 0,
        tpa: 0,
        ftm: 0,
        fta: 0,
        orb: 0,
        drb: 0,
        ast: 0,
        stl: 0,
        blk: 0,
        tov: 0
      }
    };
  });
  
  console.log(`Processed ${players.length} players from aggregated data`);
  return players;
}

// Main execution
console.log('Processing ACB data...\n');

console.log('\nProcessing shot charts...');
const allData = loadAllGames();
const shots = processShotCharts(allData);
fs.writeFileSync(path.join(OUTPUT_DIR, 'shots.json'), JSON.stringify(shots, null, 2));
console.log(`  Saved ${shots.length} shots`);

console.log('\nProcessing team statistics...');
const teamStats = processTeamStats();
fs.writeFileSync(path.join(OUTPUT_DIR, 'teams.json'), JSON.stringify(teamStats, null, 2));
console.log(`  Saved ${teamStats.length} teams`);

console.log('\nProcessing player statistics...');
const playerStats = processPlayerStats();
fs.writeFileSync(path.join(OUTPUT_DIR, 'players.json'), JSON.stringify(playerStats, null, 2));
console.log(`  Saved ${playerStats.length} players`);

console.log('\n✓ Data processing complete!');
