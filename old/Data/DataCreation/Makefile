###########
# Make Green Book Data
# Christopher Gandrud
# 18 January 2013
###########

# Note: Requires that Stata is in your PATH.

DIR = .

# Create list of Stata source files
SSOURCE = $(wildcard $(DIR)/*.do)

# Create list of R source files
RSOURCE = $(wildcard $(DIR)/*.R)

# Files to indicate when the RSOURCE file was run
OUT_FILES1 = $(SSOURCE:.do=.log)

# Files to indicate when the RSOURCE file was run
OUT_FILES2 = $(RSOURCE:.R=.Rout)

# Default target
all: $(OUT_FILES1) $(OUT_FILES2)

# Run the SSOURCE files
$(DIR)/%.log: $(DIR)/%.do
	Stata -e do $<

# Run the RSOURCE files
$(DIR)/%.Rout: $(DIR)/%.R
	R CMD BATCH $<

# Remove Out Files
clean:
	rm -fv $(OUT_FILES1) $(OUT_FILES2)