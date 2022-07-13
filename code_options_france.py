import pandas as pd
import numpy as np
pd.options.display.width = 0
pd.options.display.float_format = '{:.4f}'.format

# times the entire code
import time
start_time = time.time()

# read entire data frame
df = pd.read_pickle('H:\\data\\France\\France_EUOptions1b')


#df.rename(columns={'#RIC':'ric', 'Date-Time':'timestamp', 'Type':'type', 'Price':'price', 'Volume':'trades', 'Bid Price':'bid', 'Ask Price':'ask', 'Ask Size':'ask_size', 'Bid Size':'bid_size'}, inplace=True)

#df = df[df.index > (17897954 - 1000000)]
#df.to_pickle('/Users/vincentwolff/Desktop/test_run_options')

# set time dimension
df['date'] = pd.to_datetime(df['timestamp'], format='%Y-%m-%d %H:%M:%S.%f', utc=True).dt.tz_localize(None)

print('direkt nach Beginn:')
print(df)
print(df.describe())



# # # # # # # # # # # # # # # # # # # # # # # # #
""" 1. create the moneyness of the options as
 the price of the underlying S over the strike of 
 the option K """
# # # # # # # # # # # # # # # # # # # # # # # # #

# needed for both, stocks and options
df1 = df.copy()

df1['time'] = df1['date'].dt.hour.astype(str).str.zfill(2) + ':' + df1['date'].dt.minute.astype(str).str.zfill(2)

# We discard trades that happen before the opening of the market so we get the real opening price
df1 = df1.loc[~(df1['time'] < '08:01')]
df1 = df1.drop(columns=['time', 'timestamp'])

# list of stocks we want to kick out of sample

list_stocks=['AIRF.PA','AKE.PA','ALSO.PA','ATOS.PA','GEPH.PA','HRMS.PA','ILD.PA','NPOS.PA','RCOP.PA','SCOR.PA','SEBF.PA','SEVI.PA','VLOF.PA','FOUG.PA','MWDP.PA']

#list_stocks=['AIRF.PA','AIRP.PA','ALSO.PA','ATOS.PA','AXAF.PA','BNPP.PA','BOUY.PA','CAPP.PA','CARR.PA','CASP.PA','SGOB.PA','MICP.PA',
 #            'CNPP.PA','CAGR.PA','DANO.PA','DAST.PA','EDF.PA','OREP.PA','LVMH.PA','MMTP.PA',
 #            'PEUP.PA','SAF.PA','SASY.PA','SCHN.PA','SCOR.PA','SOGN.PA','TFFP.PA','TCFP.PA','TOTF.PA','VLOF.PA','VLLP.PA','VIE.PA','SGEF.PA','VIV.PA']
# keep only options in list
options = df1.copy()
options = options[~options['ric'].isin(list_stocks)]
options = options[['ric']]
options = options.drop_duplicates().reset_index(drop=True)

# create the stock-option ric so we can match the underlying price and the strike
# extract from ric all the letters until one number occurs
options['comp_ric'] = options.ric.str.extract(r'^([^\d]+\d)').replace('[^A-Za-z0-9]', '', regex=True).replace('(\d)', '', regex=True)
#eliminate one more letter for european type options (elimiante if US options)
options['comp_ric'] = options['comp_ric'].str[:-1]
# for ric's with numbers with have to adjust by hand
options.loc[options['comp_ric'] == 'A', 'comp_ric'] = 'A2'
options['comp_ric'] = options['comp_ric'] + '.' + 'PA'

# create the strike price
options['strike'] = options['ric'].str.split('.').str[0]
options['strike'] = options['strike'].str[:-2]
options['strike'] = options['strike'].str.extract('(\d+)', expand=True)
options.loc[options['comp_ric'] == 'A2.MI', 'strike'] = options['strike'].str[1:]
options['strike'] = options['strike'].astype(float, errors='raise')
options['strike'] = options['strike'] / 1000
options.rename(columns={'ric': 'security_ric', 'comp_ric': 'ric'}, inplace=True)

# keep only stocks in list
stocks = df1.copy()
stocks = stocks[stocks['ric'].isin(list_stocks)]


# resample by day and use the first value in the day for the moneyness ratio
stocks = stocks.set_index(pd.DatetimeIndex(stocks['date']))
stocks = (stocks.groupby(by=['ric']).resample('D').first()).dropna(subset=['price'])
stocks = stocks.drop(['date', 'ric'], axis=1)
stocks = stocks.reset_index()

# merge stocks and options
df1 = pd.merge(stocks, options, on='ric')

# create moneyness as ratio of price of underlying over the strike
df1['moneyness'] = df1['price'] / df1['strike']

# due to specification by the strike price of the option in the options-ric,
# we keep all options which are between 10-time multiples of a moneyness of 0.9 - 1.1
df1['one'] = df1['moneyness'].between(0.9, 1.1)
df1['one'] = df1['one'].astype(int)
df1['two'] = df1['moneyness'].between(9, 11)
df1['two'] = df1['two'].astype(int)
df1['three'] = df1['moneyness'].between(0.09, 0.11)
df1['three'] = df1['three'].astype(int)
df1['four'] = df1['moneyness'].between(90, 110)
df1['four'] = df1['four'].astype(int)
df1['new'] = df1['one'] + df1['two'] + df1['three'] + df1['four']
df1 = df1[df1['new'] > 0]
df1 = df1.drop(columns=['one', 'two', 'three', 'four', 'new'])

print('direkt nach definition moneyness:')
print(df1)
print(df1.describe())

df1 = df1[['date', 'security_ric']].reset_index(drop=True)
df1.rename(columns={'security_ric': 'ric', 'date': 'date_daily'}, inplace=True)

#options.to_excel('H:/options.xlsx')
#stocks.to_excel('H:/stockss.xlsx')






# # # # # # # # # # # # # # # # # # # # # # # # #
""" 2. merge the entire data frame on the 
options with moneyness between 0.9 and 1.1 """
# # # # # # # # # # # # # # # # # # # # # # # # #

df['date_daily'] = pd.to_datetime(df['date'].dt.date)

# by merging we drop all day-option tuples with a moneyness below 0.9 or above 1.1
df = pd.merge(df1, df, on=['date_daily', 'ric'])

# We discard trades that are traded outside of trading hours.
# it is in London time, therefore we go from 8:00 to 16:30 instead of 9:00 to 17:30
df['time'] = df['date'].dt.hour.astype(str).str.zfill(2) + ':' + df['date'].dt.minute.astype(str).str.zfill(2)
df = df.loc[~(df['time'] < '08:01')]
df = df.loc[~(df['time'] > '16:30')]

# generate the mid price
df['mid'] = (df['bid'] + df['ask']) / 2

# drop all options with a quote mid price smaller than 0.01
# tick size for options trading is:
# Tick SIze: 0.0005 for prices > 0.05
# Tick SIze: 0.0001 for prices < 0.05
# df = df[df['bid'] > 0]

# generate variables
df = df[['ric', 'date_daily', 'date', 'type', 'trades', 'price', 'mid', 'bid', 'bid_size', 'ask', 'ask_size']]
df = df.reset_index(drop=True)

print('direkt nach drop by moneyness :')
print(df)
print(df.describe())







# # # # # # # # # # # # # # # # # # # # # # # # #
""" 3. create the maturity of all options after
 controlling for the right moneyness"""
# # # # # # # # # # # # # # # # # # # # # # # # #

df2 = df.copy()
df2 = df2[['ric', 'date_daily']]
df2 = df2.drop_duplicates().reset_index(drop=True)

# get the
df2['one'] = df2['ric'].str.split('.').str[0].str[-2:]
df2['expiry_date'] = 'NaN'

# define expiration day for 2012 Call Options
df2.loc[df2['one'] == 'A2', 'expiry_date'] = '2012-01-20'
df2.loc[df2['one'] == 'B2', 'expiry_date'] = '2012-02-17'
df2.loc[df2['one'] == 'C2', 'expiry_date'] = '2012-03-16'
df2.loc[df2['one'] == 'D2', 'expiry_date'] = '2012-04-20'
df2.loc[df2['one'] == 'E2', 'expiry_date'] = '2012-05-18'
df2.loc[df2['one'] == 'F2', 'expiry_date'] = '2012-06-15'
df2.loc[df2['one'] == 'G2', 'expiry_date'] = '2012-07-20'
df2.loc[df2['one'] == 'H2', 'expiry_date'] = '2012-08-17'
df2.loc[df2['one'] == 'I2', 'expiry_date'] = '2012-09-21'
df2.loc[df2['one'] == 'J2', 'expiry_date'] = '2012-10-19'
df2.loc[df2['one'] == 'K2', 'expiry_date'] = '2012-11-16'
df2.loc[df2['one'] == 'L2', 'expiry_date'] = '2012-12-21'

# define expiration day for 2013 Call Options
df2.loc[df2['one'] == 'A3', 'expiry_date'] = '2013-01-18'
df2.loc[df2['one'] == 'B3', 'expiry_date'] = '2013-02-15'
df2.loc[df2['one'] == 'C3', 'expiry_date'] = '2013-03-15'
df2.loc[df2['one'] == 'D3', 'expiry_date'] = '2013-04-19'
df2.loc[df2['one'] == 'E3', 'expiry_date'] = '2013-05-17'
df2.loc[df2['one'] == 'F3', 'expiry_date'] = '2013-06-21'
df2.loc[df2['one'] == 'G3', 'expiry_date'] = '2013-07-19'
df2.loc[df2['one'] == 'H3', 'expiry_date'] = '2013-08-16'
df2.loc[df2['one'] == 'I3', 'expiry_date'] = '2013-09-20'
df2.loc[df2['one'] == 'J3', 'expiry_date'] = '2013-10-18'
df2.loc[df2['one'] == 'K3', 'expiry_date'] = '2013-11-15'
df2.loc[df2['one'] == 'L3', 'expiry_date'] = '2013-12-20'


# define expiration day for 2012 Put Options
df2.loc[df2['one'] == 'A2', 'expiry_date'] = '2012-01-20'
df2.loc[df2['one'] == 'B2', 'expiry_date'] = '2012-02-17'
df2.loc[df2['one'] == 'C2', 'expiry_date'] = '2012-03-16'
df2.loc[df2['one'] == 'D2', 'expiry_date'] = '2012-04-20'
df2.loc[df2['one'] == 'E2', 'expiry_date'] = '2012-05-18'
df2.loc[df2['one'] == 'F2', 'expiry_date'] = '2012-06-15'
df2.loc[df2['one'] == 'G2', 'expiry_date'] = '2012-07-20'
df2.loc[df2['one'] == 'H2', 'expiry_date'] = '2012-08-17'
df2.loc[df2['one'] == 'I2', 'expiry_date'] = '2012-09-21'
df2.loc[df2['one'] == 'J2', 'expiry_date'] = '2012-10-19'
df2.loc[df2['one'] == 'K2', 'expiry_date'] = '2012-11-16'
df2.loc[df2['one'] == 'L2', 'expiry_date'] = '2012-12-21'

# define expiration day for 2013 Put Options
df2.loc[df2['one'] == 'A3', 'expiry_date'] = '2013-01-18'
df2.loc[df2['one'] == 'B3', 'expiry_date'] = '2013-02-15'
df2.loc[df2['one'] == 'C3', 'expiry_date'] = '2013-03-15'
df2.loc[df2['one'] == 'D3', 'expiry_date'] = '2013-04-19'
df2.loc[df2['one'] == 'E3', 'expiry_date'] = '2013-05-17'
df2.loc[df2['one'] == 'F3', 'expiry_date'] = '2013-06-21'
df2.loc[df2['one'] == 'G3', 'expiry_date'] = '2013-07-19'
df2.loc[df2['one'] == 'H3', 'expiry_date'] = '2013-08-16'
df2.loc[df2['one'] == 'I3', 'expiry_date'] = '2013-09-20'
df2.loc[df2['one'] == 'J3', 'expiry_date'] = '2013-10-18'
df2.loc[df2['one'] == 'K3', 'expiry_date'] = '2013-11-15'
df2.loc[df2['one'] == 'L3', 'expiry_date'] = '2013-12-20'




# calculate time to maturity
df2['expiry_date'] = pd.to_datetime(df2['expiry_date'])
df2['diff'] = df2['expiry_date'] - df2['date_daily']
df2 = df2.dropna(subset=['diff'])

# drop if maturity is longer than 250 days
# or shorter than 7
df2 = df2[df2['diff'] >= '7 days']
df2 = df2[df2['diff'] <= '250 days']
df2 = df2[['ric', 'date_daily']]

df = pd.merge(df2, df, on=['date_daily', 'ric'])

print('direkt nach df2 merge mit df:')
print(df)
print(df.describe())

# set time dimension
#df['date'] = pd.to_datetime(df['date'], format='%Y-%m-%d %H:%M:%S.%f', utc=True).dt.tz_localize(None)
df = df.set_index(pd.DatetimeIndex(df['date']))



# df.to_pickle('/Users/vincentwolff/Desktop/test_run_options')





""" Trading activity """
# We measure trading activity as the natural logarithm
# of the EUR value traded for a stock option by aggregating
# all trading volume over the underlying stock i on day t
activity = df.copy()
activity['volume'] = activity['trades'] * activity['price']
activity = activity[['ric', 'date_daily', 'trades', 'volume']]

# drop all non-trades rows
activity = activity.dropna(subset=['trades'])

# extract the company RIC
activity['underlying_ric'] = activity.ric.str.extract(r'^([^\d]+\d)').replace('[^A-Za-z0-9]', '', regex=True).replace('(\d)', '', regex=True)
# for ric's with numbers with have to adjust by hand
activity.loc[activity['underlying_ric'] == 'A', 'underlying_ric'] = 'A2'
# add the exchange identifier
activity['underlying_ric'] = activity['underlying_ric'] + '.' + 'PA'

# generate daily option volume by the option's underlying stock
activity_volume = activity.groupby(by=['underlying_ric', 'date_daily'])['volume'].sum().reset_index()

# generate daily option trades by the option's underlying stock
activity_trades = activity.groupby(by=['underlying_ric', 'date_daily'])['trades'].sum().reset_index()






""" Proportional intra-day quoted bid-ask spread """
p_q_b_a = df.copy()
# drop trade data
p_q_b_a = p_q_b_a.loc[~(p_q_b_a['type'] == 'Trade')]
p_q_b_a = p_q_b_a.dropna(subset=['mid'])
p_q_b_a['depth'] = p_q_b_a['bid'] * p_q_b_a['bid_size'] + p_q_b_a['ask'] * p_q_b_a['ask_size']
p_q_b_a['relative_spread'] = (p_q_b_a['ask'] - p_q_b_a['bid']) / p_q_b_a['mid']
p_q_b_a = p_q_b_a[['ric', 'date_daily', 'mid', 'relative_spread', 'depth']]

depth = p_q_b_a.copy()
depth = depth[['ric', 'date_daily', 'depth']]

# resample the data so we have daily measures for price_impact and realized_spread
depth = (depth.groupby(by=['ric', 'date_daily']).sum()).reset_index().rename(columns={'depth': 'total_depth'})
p_q_b_a = pd.merge(p_q_b_a, depth, on=['ric', 'date_daily'], how='left')

# generate the weighted intra-day bid-ask spread
p_q_b_a['intra_day_bid_ask'] = ((p_q_b_a['relative_spread'] * p_q_b_a['depth']) / p_q_b_a['total_depth'])
p_q_b_a = p_q_b_a[['date_daily', 'ric', 'intra_day_bid_ask']]
p_q_b_a = (p_q_b_a.groupby(by=['ric', 'date_daily']).sum()).reset_index()






""" Proportional end-of-day quoted bid-ask spread """
# we weight end of day bid-ask spreads by the relative depth
# of the specific stock optionen relative to the overall underlying stocks depth
bidask_daily = df.copy()

# drop all trades
bidask_daily = bidask_daily.loc[~(bidask_daily['type'] == 'Trade')]
bidask_daily = bidask_daily[['ric', 'date_daily', 'mid', 'bid', 'ask', 'bid_size', 'ask_size']]
# drop if either bid or ask is NaN
bidask_daily = bidask_daily.dropna(subset=['mid'])

# resample to get end of day closing quoted bid and ask spreads
bidask_daily = (bidask_daily.groupby(by=['ric', 'date_daily']).resample('D').last())
bidask_daily = bidask_daily[['mid', 'bid', 'ask', 'bid_size', 'ask_size']].reset_index().drop(['date'], axis=1)

# create the relative bid ask spread
bidask_daily['bid_ask'] = (bidask_daily['ask'] - bidask_daily['bid']) / bidask_daily['mid']

# create the depth at the end of the day
bidask_daily['depth'] = (bidask_daily['bid_size'] + bidask_daily['ask_size'])

# drop if the quoted mid price is smaller or equal to the bid ask spread
bidask_daily = bidask_daily[bidask_daily['bid_ask'] < 1.00]
bidask_daily = bidask_daily[['ric', 'date_daily', 'depth', 'bid_ask']]

# generate the options underlying stock RIC
bidask_daily['underlying_ric'] = bidask_daily.ric.str.extract(r'^([^\d]+\d)').replace('[^A-Za-z0-9]', '', regex=True).replace('(\d)', '', regex=True)
# for ric's with numbers with have to adjust by hand
bidask_daily.loc[bidask_daily['underlying_ric'] == 'A', 'underlying_ric'] = 'A2'
# add the exchange identifier
bidask_daily['underlying_ric'] = bidask_daily['underlying_ric'] + '.' + 'PA'

# generate daily option volume by the option's underlying stock
depth = bidask_daily.groupby(by=['underlying_ric', 'date_daily'])['depth'].sum().reset_index().rename(columns={'depth': 'total_depth'})
bidask_daily = pd.merge(bidask_daily, depth, on=['underlying_ric', 'date_daily'], how='left')

# generate the weighted end of day bid ask spread
bidask_daily['end_of_day_bid_ask'] = bidask_daily['bid_ask'] * (bidask_daily['depth'] / bidask_daily['total_depth'])

# generate the weighted end of day bid ask spread as a mean over all options
bidask_daily = bidask_daily.groupby(by=['underlying_ric', 'date_daily'])['end_of_day_bid_ask'].sum().reset_index()#.rename(columns={'depth': 'total_depth'})





""" Volatilities """
""" Range for each Option"""
# range is the intraday price range across all trades, normalized by the average traded price.
# it is expressed in percentage points.
range_1 = df.copy()

# (max price - min price) / average price
range_1 = ((range_1.groupby(by=['ric', 'date_daily'])['mid'].max() - range_1.groupby(by=['ric', 'date_daily'])['mid'].min()) / range_1.groupby(by=['ric', 'date_daily'])['mid'].mean())
range_1 = pd.DataFrame(data=range_1).rename(columns={'mid': 'range_1'}).reset_index()







""" Range aggregated by underlying stock"""
# calculate average range measure for the underlying stock
range_2 = range_1.copy()

# extract the ric of the underlying stock
range_2['underlying_ric'] = range_2.ric.str.extract(r'^([^\d]+\d)').replace('[^A-Za-z0-9]', '', regex=True).replace('(\d)', '', regex=True)
# for ric's with numbers with have to adjust by hand
range_2.loc[range_2['underlying_ric'] == 'A', 'underlying_ric'] = 'A2'
# add the exchange identifier
range_2['underlying_ric'] = range_2['underlying_ric'] + '.' + 'PA'

# calculate the average
range_2 = range_2.groupby(by=['underlying_ric', 'date_daily'])['range_1'].mean().reset_index().rename(columns={'range_1': 'range_2'})





""" Realized Volatility for each option """
real_vol_1 = df.copy()
# generate mid price
real_vol_1['mid'] = (real_vol_1['bid'] + real_vol_1['ask']) / 2
real_vol_1 = real_vol_1[['ric', 'mid', 'date_daily']]
real_vol_1 = real_vol_1.dropna(subset=['mid'])

# resample mid price by 5 minutes
real_vol_1 = (real_vol_1.groupby(by=['ric', 'date_daily']).resample('5min').last()).dropna(subset=['mid'])
real_vol_1 = real_vol_1.drop(['ric', 'date_daily'], axis=1)
real_vol_1 = real_vol_1.reset_index()

# generate squared returns by ric
real_vol_1['ret'] = ((real_vol_1.groupby(by=['ric', 'date_daily'])['mid'].pct_change()) ** 2)
real_vol_1 = real_vol_1.drop(['mid'], axis=1).reset_index().dropna(subset=['ret'])
real_vol_1 = real_vol_1[['ric', 'date_daily', 'ret']]

real_vol_1 = pd.DataFrame(real_vol_1.groupby(by=['ric', 'date_daily'])['ret'].sum()).reset_index().rename(columns={'ret': 'realized_vola_1'})
real_vol_1['realized_vola_1'] = (real_vol_1['realized_vola_1'] * np.sqrt(252)) * 100




""" Realized Volatility by underlying stock"""
real_vol_2 = real_vol_1.copy()
# extract the ric of the underlying stock
real_vol_2['underlying_ric'] = real_vol_2.ric.str.extract(r'^([^\d]+\d)').replace('[^A-Za-z0-9]', '', regex=True).replace('(\d)', '', regex=True)
# for ric's with numbers with have to adjust by hand
real_vol_2.loc[real_vol_2['underlying_ric'] == 'A', 'underlying_ric'] = 'A2'
# add the exchange identifier
real_vol_2['underlying_ric'] = real_vol_2['underlying_ric'] + '.' + 'PA'

# calculate the average
real_vol_2 = real_vol_2.groupby(by=['underlying_ric', 'date_daily'])['realized_vola_1'].mean().reset_index().rename(columns={'realized_vola_1': 'realized_vola_2'})



""" Amihud illiquidity measure """
amihud = df.copy()
amihud = amihud[['ric', 'date_daily', 'mid', 'trades', 'price']]

# resample to get end of day closing quoted bid and ask spreads
amihud_daily = (amihud.groupby(by=['ric', 'date_daily'])['mid'].resample('D').last()).reset_index().rename(columns={'mid': 'mid_daily'}).drop(['date'], axis=1)

# shift by one day
amihud_daily['mid_daily_lag_1'] = amihud_daily['mid_daily'].shift(periods=1)
amihud = pd.merge(amihud, amihud_daily, on=['ric', 'date_daily'], how='left')

amihud = amihud.dropna(subset=['price'])

amihud['volume'] = amihud['trades'] * amihud['price']
amihud['amihud'] = (abs((amihud['price'] - amihud['mid_daily_lag_1']) / amihud['mid_daily_lag_1'])) / amihud['volume']

# extract the company RIC
amihud['underlying_ric'] = amihud.ric.str.extract(r'^([^\d]+\d)').replace('[^A-Za-z0-9]', '', regex=True).replace('(\d)', '', regex=True)
# for ric's with numbers with have to adjust by hand
amihud.loc[amihud['underlying_ric'] == 'A', 'underlying_ric'] = 'A2'
# add the exchange identifier
amihud['underlying_ric'] = amihud['underlying_ric'] + '.' + 'PA'
amihud = amihud[['underlying_ric', 'date_daily', 'amihud']]
amihud = amihud.groupby(by=['underlying_ric', 'date_daily'])['amihud'].sum().reset_index()

print('amihud')
print(amihud)




""" Quoted depth """
# drop trade data
depth = df.copy()
depth = depth.loc[~(depth['type'] == 'Trade')]
depth['depth'] = depth['bid'] * depth['bid_size'] + depth['ask'] * depth['ask_size']

depth = depth.copy()
depth = depth[['ric', 'date_daily', 'depth']]
# resample the data so we have daily measures for price_impact and realized_spread
depth = (depth.groupby(by=['ric', 'date_daily']).sum()).reset_index().rename(columns={'depth': 'depth'})

print('depth')
print(depth)


# merge all the data
df = df[['ric', 'date_daily']]
df = df.reset_index().drop(['date'], axis=1)
df = df.drop_duplicates().reset_index().drop(['index'], axis=1)

df['underlying_ric'] = df.ric.str.extract(r'^([^\d]+\d)').replace('[^A-Za-z0-9]', '', regex=True).replace('(\d)', '', regex=True)
# for ric's with numbers with have to adjust by hand
df.loc[df['underlying_ric'] == 'A', 'underlying_ric'] = 'A2'
# add the exchange identifier
df['underlying_ric'] = df['underlying_ric'] + '.' + 'PA'
df = df[['underlying_ric', 'ric', 'date_daily']]

df = pd.merge(df, activity_volume, on=['underlying_ric', 'date_daily'], how='left')
df = pd.merge(df, activity_trades, on=['underlying_ric', 'date_daily'], how='left')
df = pd.merge(df, p_q_b_a, on=['ric', 'date_daily'], how='left')
df = pd.merge(df, bidask_daily, on=['underlying_ric', 'date_daily'], how='left')
df = pd.merge(df, range_1, on=['ric', 'date_daily'], how='left')
df = pd.merge(df, range_2, on=['underlying_ric', 'date_daily'], how='left')
df = pd.merge(df, real_vol_1, on=['ric', 'date_daily'], how='left')
df = pd.merge(df, real_vol_2, on=['underlying_ric', 'date_daily'], how='left')
df = pd.merge(df, amihud, on=['underlying_ric', 'date_daily'], how='left')
df = pd.merge(df, depth, on=['ric', 'date_daily'], how='left')

print(df)
print(df.describe())
print(df.shape)
df.to_excel('H:/data/Liquiditymeasures/Options/Liquiditymeasures_France_EUREX.xlsx')
print("--- %s seconds ---" % (time.time() - start_time))

