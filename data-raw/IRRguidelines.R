IRRguidelines <- list(
	'Cicchetti' = list(
		breaks = c('Poor'=0,'Fair'=0.4,'Good'=0.6,'Excellent'=0.75),
		metrics = c('ICC', 'Cohen Kappa'),
		reference = 'Cicchetti & Sparrow (1981); Cicchetti (2001)'),
	'Zeger' = list(
		breaks = c('Slight'=0,'Fair'=0.2,'Moderate'=0.4,'Substantial'=0.6,'Almost perfect'=0.8),
		metrics = c('Cohen Kappa'),
		reference = 'Zeger et al. (2010)'),
	'Fleiss' = list(
		breaks = c('Poor'=0,'Fair'=0.4,'Excellent'=0.75),
		metrics = c('Cohen Kappa'),
		reference = 'Fleiss (1981, 1986); Brage et al. (1998); Martin et al. (1997); Svanholm et al. (1989)'),
	'Altman' = list(
		breaks = c('Poor'=0,'Fair'=0.2,'Moderate'=0.4,'Good'=0.6,'Very good'=0.8),
		metrics = c(''),
		reference = 'Altman (1990)'),
	'Shrout' = list(
		breaks = c('Virtually none'=0,'Slight'=0.1,'Fair'=0.4,'Moderate'=0.6,'Substantial'=0.8),
		metrics = c(''),
		reference = 'Shrout (1998)'),
	'Landis and Koch' = list(
		breaks = c('Poor'=0,'Fair'=0.2,'Moderate'=0.4,'Substantial'=0.6,'Almost perfect'=0.8),
		metrics = 'Cohen Kappa',
		reference = 'Landis & Koch (1997)'),
	'Portney and Watkins' = list(
		breaks = c('Poor to moderate'=0,'Reasonable for clinical measurement'=0.75),
		metrics = c('ICC'),
		reference = 'Portney & Watkins (2009)'),
	'Koo and Li' = list(
		breaks = c('Poor'=0,'Moderate'=0.5,'Good'=0.75,'Excellent'=0.9),
		metrics = c('ICC'),
		reference = 'Koo & Li (2016)')
)
save(IRRguidelines, file = 'data/IRRguidelines.Rda')
