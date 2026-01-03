# RIBITSr Development Workflow with Claude Code

Complete guide to the continuous learning loop for RIBITSr development.

## 🔄 The Learning Loop

Your workflow implements: **Build → Test → Learn → Repeat**

```
┌──────────────────────────────────────────────────────────────┐
│                   CONTINUOUS LEARNING                         │
└──────────────────────────────────────────────────────────────┘

    /advise                    Work                /retrospective
       ↓                         ↓                        ↓
┌──────────────┐        ┌──────────────┐        ┌──────────────┐
│  BEFORE      │        │   DURING     │        │   AFTER      │
│              │        │              │        │              │
│ • Query      │   →    │ • Build      │   →    │ • Analyze    │
│   skills     │        │ • Test       │        │   session    │
│ • See past   │        │ • Narrate    │        │ • Extract    │
│   failures   │        │ • Document   │        │   learnings  │
│ • Get plan   │        │              │        │ • Save skill │
└──────────────┘        └──────────────┘        └──────────────┘
       ↑                                                 ↓
       └─────────────────────────────────────────────────┘
                    New skill available next time!
```

## Step-by-Step Guide

### 1️⃣ Before Work: `/advise`

Before starting a task, query past learnings:

```bash
/advise [what you're about to work on]
```

**Examples:**
```bash
/advise refactoring transaction date parsing
/advise adding new EPA API endpoint
/advise improving error messages in ribits()
/advise debugging spatial data harmonization
```

**What you get:**
- 📚 **Relevant skills** from `.claude/skills/`
- ✅ **What worked** in similar past tasks
- ❌ **Known failures** (critical - avoid repeating mistakes!)
- ⚙️ **Working configurations** from successful sessions
- 🎯 **Recommended approach** based on all of the above
- 📋 **Pre-work checklist**

**Example output:**
```
📚 Relevant Skills
- datetime-handling: Parse EPA Unix timestamps correctly
- tidyverse-refactoring: Convert loops to purrr::map

✅ What Has Worked
- Using as.POSIXct() with explicit tz = "UTC"
- purrr::map_df() for transaction harmonization

❌ Known Failures (Avoid These!)
- DON'T use as.POSIXct() without timezone → causes user-dependent bugs
- DON'T use base merge() for three-way joins → creates duplicates
- DON'T parse pre-2000 dates without special handling → wrong years

⚙️ Working Configurations
- source_priority = c("csv", "api", "epa") for conflict resolution
- rb_config(use_persistent_cache = TRUE) for development

🎯 Recommended Approach
1. Start with R/harmonization-handling.R:parse_unix_timestamp()
2. Use lubridate::as_datetime() with tz = "UTC"
3. Check for pre-2000 threshold (< 946684800)
4. Test with dates from 1998, 2000, 2021
```

### 2️⃣ During Work: Narrate & Document

As you work, **narrate your thinking out loud** to Claude. This is crucial for `/retrospective`!

**Good narration examples:**

```
"I'm trying purrr::map_df here instead of a for loop because it's more
idiomatic and handles the data frame binding automatically. Let me test it..."

"This failed with 'cannot coerce type list to vector of type double'.
I think the issue is that some timestamps are NULL. I'll use map_dbl with
a default value instead... No wait, that won't work either because we want
POSIXct not double. Let me use map with safely() to handle errors..."

"Found the issue! Pre-2000 dates were using a different epoch. I need to
add a threshold check at 946684800 (Jan 1, 2000). This is documented in
the harmonization-handling.R file."

"This approach worked! Using case_when() made the logic much clearer than
nested ifelse statements."
```

**Why narrate?**
- `/retrospective` reads the conversation to extract learnings
- Better narration → better auto-generated skills
- Documents your decision-making process
- Captures why you chose one approach over another

**What to document:**
- ✅ Approaches you try (successful and failed)
- ✅ Why you chose specific solutions
- ✅ Trade-offs you considered
- ✅ Discoveries about EPA APIs or data
- ✅ Error messages and how you resolved them
- ✅ Edge cases you encountered

### 3️⃣ After Work: `/retrospective`

When you've completed a task or made significant progress, capture learnings:

```bash
/retrospective
```

**What happens:**
1. Claude reads the **entire conversation**
2. Identifies what you were trying to accomplish
3. Extracts **successful patterns** with code examples
4. Documents **failed approaches** (very important!)
5. Distills **key learnings**
6. Generates a new skill file using `.claude/templates/r-package-skill-template/`
7. Saves to `.claude/skills/[new-skill-name]/SKILL.md`

**Example output:**
```
📝 Retrospective Summary

Skill Created: pre-2000-timestamp-handling
Location: .claude/skills/pre-2000-timestamp-handling/SKILL.md

What Worked:
- Threshold check at 946684800 (Jan 1, 2000)
- Special handling function in harmonization-handling.R
- Using lubridate::as_datetime() with explicit UTC

What Failed (valuable learning!):
- Using as.POSIXct() without threshold → Pre-2000 dates showed as 2038+
- Base R merge() on three sources → Created duplicate rows
- Parsing without timezone → User-dependent results

Key Learnings:
- EPA uses Unix timestamps but pre-2000 dates need correction
- Always specify tz = "UTC" explicitly for EPA data
- Test with dates spanning 1998-2025 range

Next Steps:
- Commit this skill to git to share with team
- The skill will auto-activate in future sessions
- Update CLAUDE.md with notes about timestamp handling
```

### 4️⃣ Commit and Share

Add the new skill to version control:

```bash
# Review the generated skill
cat .claude/skills/[new-skill-name]/SKILL.md

# Add to git
git add .claude/skills/
git commit -m "docs: Add [skill-name] from retrospective"

# Share with team
git push origin main
```

**Team benefits:**
- Everyone learns from your discoveries
- Known failures documented (saves team time)
- Working solutions readily available

### 5️⃣ Next Session: Improved Context

Next time you (or a teammate) work on a similar task:
- `/advise` now includes this skill's learnings
- Claude auto-triggers the skill when relevant
- Known failures are surfaced early
- Continuous improvement!

## 📋 Quick Reference

| Command | When | Purpose |
|---------|------|---------|
| `/advise [topic]` | **Before** work | Get guidance from past learnings |
| *Narrate thinking* | **During** work | Document for retrospective |
| `/retrospective` | **After** work | Capture learnings as skill |

## 🎯 Key Principles

### 1. Document Failures Prominently

**Failures are as valuable as successes!**

They tell you:
- Which paths to skip entirely
- Common mistakes to avoid
- Why obvious approaches don't work
- Edge cases to watch for

Always include "What Failed" sections with:
- What you tried
- Why it failed
- What to do instead

### 2. Narrate Your Thinking

The better you narrate during work, the better `/retrospective` can extract patterns.

**Good:** "I'm using purrr::map_df instead of a loop because..."
**Better:** "I tried a loop but got memory issues with 10k+ transactions, so switching to purrr::map_df for better performance..."

### 3. Be Specific to RIBITSr

Skills should reference:
- Actual files (e.g., `R/harmonization-handling.R:234`)
- Real functions (e.g., `parse_unix_timestamp()`)
- Specific problems (e.g., "EPA ArcGIS only has approved banks")
- Concrete solutions (e.g., "source_priority = c('csv', 'api', 'epa')")

### 4. Continuous Improvement

Each session adds to the knowledge base:
- Session 1: Discover pattern
- Session 2: Build on pattern, discover edge case
- Session 3: Refine approach, document anti-patterns
- Session 4+: Have comprehensive guidance available

## 💡 Tips for Success

### Make `/retrospective` Better

- Narrate decisions: "Choosing X over Y because..."
- Document errors: "Got error: [paste error], fixed by..."
- Explain discoveries: "Found that EPA data has..."
- Note surprises: "Unexpectedly, pre-2000 dates..."

### Get More from `/advise`

- Be specific: `/advise parsing CSV transaction dates` vs `/advise dates`
- Ask about domains: `/advise EPA API quirks`
- Request comparisons: `/advise best approach for harmonizing 3 sources`

### Skills Auto-Trigger

You don't need to explicitly invoke skills. When you say:
- "Help me document this function" → `roxygen2-documentation` activates
- "Refactor this loop to tidyverse" → `tidyverse-refactoring` activates
- "Parse this timestamp" → `datetime-handling` activates

## 📚 Resources

- **CLAUDE.md** - Project context, architecture, patterns (loaded every session)
- **.claude/skills/** - Auto-triggered skills (5 pre-built + your additions)
- **.claude/commands/** - Slash commands (`/advise`, `/retrospective`)
- **.claude/templates/** - Template for auto-generating skills

## 🚀 Example Session

```bash
# ─────────────────────────────────────────────────────────
# BEFORE WORK
# ─────────────────────────────────────────────────────────

You: /advise refactoring transaction date parsing

Claude: [Provides guidance from datetime-handling skill, CLAUDE.md,
        and any prior sessions on date parsing. Shows known failures
        like "don't parse without timezone", working solutions like
        "use threshold at 946684800", etc.]

# ─────────────────────────────────────────────────────────
# DURING WORK
# ─────────────────────────────────────────────────────────

You: Help me refactor the date parsing in transactions-harmonize.R.
     I see we're using a loop here, and there's no timezone handling.

     *Shows code with for loop and as.POSIXct without tz*

Claude: [datetime-handling skill auto-triggers]

     I'll help refactor this using tidyverse patterns with proper
     timezone handling. Based on the datetime-handling skill...

You: Okay I tried this approach:

     data %>% mutate(date = as_datetime(timestamp, tz = "UTC"))

     But I'm getting errors for some old dates from the 1990s.

Claude: This is the pre-2000 date issue documented in datetime-handling.
        You need the threshold check...

You: That worked! I added the threshold check and now dates from 1998
     parse correctly. Should I add a test for this?

Claude: Yes, definitely test edge cases...

# ─────────────────────────────────────────────────────────
# AFTER WORK
# ─────────────────────────────────────────────────────────

You: /retrospective

Claude: [Analyzes conversation, extracts:]
        - Used tidyverse refactoring (mutate + as_datetime)
        - Discovered pre-2000 date issue
        - Fixed with threshold check at 946684800
        - Added tests for 1998, 2000, 2021 dates

        [Creates new skill or updates existing datetime-handling skill]

You: git add .claude/skills/
     git commit -m "docs: Update datetime-handling with pre-2000 pattern"
     git push

# ─────────────────────────────────────────────────────────
# NEXT SESSION - Knowledge Persists!
# ─────────────────────────────────────────────────────────

Teammate: /advise parsing EPA timestamps

Claude: [Now includes your learnings about pre-2000 dates!]
```

## Summary

Your continuous learning loop:

1. **`/advise`** → Learn from past (failures + successes)
2. **Narrate** → Document thinking during work
3. **`/retrospective`** → Capture learnings as skills
4. **Repeat** → Build institutional knowledge over time

**The key:** Document failures prominently. They're learning opportunities that save everyone time!

Happy learning! 🎉
