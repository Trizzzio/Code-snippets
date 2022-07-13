# -*- coding: utf-8 -*-
"""
###############################################################################
This codes belongs to Patrick Thöni and Vincent Wolff.
Version: 20.07.2021
vincent.wolff@bf.uzh.ch
This is the code for Eikon Refinitiv Trade and Quote Data.
This codes generates daily market quality and liquidity measures from TAQ data.
###############################################################################
Thöni, Patrick and Vincent Wolff (2021). Taxing Two Markets. Journal of XXXX
Thöni, Patrick and Vincent Wolff (2021). Trading in the Presence of Transaction Taxes – a Tale of Two Markets. Journal of XXXX
Thöni, Patrick and Vincent Wolff (2021). Trading in the Presence of Transaction Costs – a Tale of Two Markets. Journal of XXXX
###############################################################################
"""

import pandas as pd
import numpy as np
pd.options.display.width = 0
pd.options.display.float_format = '{:.4f}'.format

# times the entire code
import time
start_time = time.time()


"""
This is the total file to calculate the daily liquidity and
market quality measures from the Refinitiv Eikon TAQ Data
1. we upload the data and drop all options
2. we clean the stock data
3. we create the market quality measures which are only generate with trade data
4. we create the market quality measures which are generate with trade and quote data
"""








# # # # # # # # # # # # # # # # # # # # # # # # #
""" 1. we upload the data and drop all options """
# # # # # # # # # # # # # # # # # # # # # # # # #

df = pd.read_pickle('H:\data\France\FranceStocks2b')





# # # # # # # # # # # # # # # # # # # # # # # # #
""" 2. we clean the stock data """
# # # # # # # # # # # # # # # # # # # # # # # # #
# rename
#df.rename(columns={'#RIC':'ric', 'Date-Time':'timestamp', 'Type':'type', 'Price':'price', 'Volume':'trades', 'Bid Price':'bid', 'Ask Price':'ask', 'Ask Size':'ask_size', 'Bid Size':'bid_size', 'Qualifiers':'qualifiers', 'Date':'date_daily'}, inplace=True)
# drop what we do not need
df = df.drop(['Domain'], axis=1)

# bid and ans prices are NAN if trade
# therefore I fill bid and ask prices with last spread before trade
df['bid'] = df['bid'].fillna(method='ffill')
df['ask'] = df['ask'].fillna(method='ffill')

# drop all negative bid-ask spreads
df = df[df['ask'] > df['bid']]

# set time dimension
df['date'] = pd.to_datetime(df['timestamp'], format='%Y-%m-%d %H:%M:%S.%f', utc=True).dt.tz_localize(None)
df['time'] = df['date'].dt.hour.astype(str).str.zfill(2) + ':' + df['date'].dt.minute.astype(str).str.zfill(2)

# We discard trades that are traded outside of trading hours.
# https://www.borsaitaliana.it/borsaitaliana/calendario-e-orari-di-negoziazione/calendario-borsa-orari-di-negoziazione.en.htm
# it is in London time, therefore we go from 8:00 to 16:30 instead of 9:17:30
df = df.loc[~(df['time'] < '08:01')]
df = df.loc[~(df['time'] > '16:30')].reset_index()

# generate date without time dimesions
df['date_daily'] = pd.to_datetime(df['date'].dt.date)
df = df.set_index(pd.DatetimeIndex(df['date']))


# here we generate a second data set which we use later for all the quote and trade market quality measures
df_quotes = df.copy()
#df.to_pickle('/Users/vincentwolff/Desktop/test.pkl')

# drop what we do not need
df = df.drop(['bid_size', 'ask_size'], axis=1)

# drop quote data and only keep trade data
# and drop data where even though it says there was a trade, no price is quoted
df = df.dropna(subset=['price'])

# generate variables
df = df[['ric', 'date', 'date_daily', 'price', 'bid', 'ask', 'trades']]

# create the date of the day without the time stamp
df = df.set_index(pd.DatetimeIndex(df['date']))
print('Final DF before measures' % df)









# # # # # # # # # # # # # # # # # # # # # # # # #
""" 3. we create the market quality measures
which are only generate with trade data """
# # # # # # # # # # # # # # # # # # # # # # # # #





""" Trading activity """
# We measure trading activity as the natural logarithm of the EUR value traded in stock i on day t
activity = df.copy()
activity = activity[['ric', 'trades', 'price']]
activity['trading_activity'] = (activity['trades'] * activity['price'])
activity = activity.groupby(by=['ric'])['trading_activity'].resample('D').sum()
activity = activity.reset_index().rename(columns={'date': 'date_daily'})





""" Volatilities """
""" Range """
# range is the intraday price range across all trades, normalized by the average traded price.
# it is expressed in percentage points.
range = df.copy()

# (max price - min price) / average price
range = ((df.groupby(by=['ric', 'date_daily'])['price'].max() - df.groupby(by=['ric', 'date_daily'])['price'].min()) / df.groupby(by=['ric', 'date_daily'])['price'].mean()) * 100
range = pd.DataFrame(data=range).rename(columns={'price': 'range'}).reset_index()





""" Realized Volatility """
real_vol = df.copy()
# generate mid price
real_vol['mid'] = (real_vol['bid'] + real_vol['ask']) / 2
real_vol['date_daily'] = real_vol['date'].dt.date
real_vol = real_vol[['ric', 'mid', 'date_daily']]

# resample mid price by 5 minutes
real_vol = (real_vol.groupby(by=['ric', 'date_daily']).resample('5min').last()).dropna(subset=['mid'])
real_vol = real_vol[['mid']]
real_vol = real_vol.reset_index()

# generate squared returns by ric and date_daily
real_vol['ret'] = ((real_vol.groupby(by=['ric', 'date_daily'])['mid'].pct_change()) ** 2)
real_vol = real_vol.drop(['mid'], axis=1).reset_index(drop=True).dropna(subset=['ret'])

# drop the ric-data_daily combination if the 5 minute return is greater than 20%
# this implies we drop the 20% * 20% = 0.04 of the squared returns
real_vol.loc[real_vol['ret'] > 0.04, 'dummy'] = 1
real_vol['dummy'] = real_vol['dummy'].fillna(0)
dumm = real_vol.groupby(by=['ric', 'date_daily']).sum().reset_index()
dumm = dumm[~(dumm['dummy'] > 0)].reset_index()
dumm = dumm[['ric', 'date_daily']]
real_vol = pd.merge(dumm, real_vol, on=['ric', 'date_daily'], how='left')
real_vol = real_vol.drop(columns=['dummy'])

# generates a daily measure of sum of squared returns
real_vol = pd.DataFrame(real_vol.groupby(by=['ric', 'date_daily'])['ret'].sum()).reset_index().rename(columns={ 'ret': 'realized_vola'})
real_vol['realized_vola'] = (real_vol['realized_vola'] * np.sqrt(252)) * 100
real_vol['date_daily'] = real_vol['date_daily'].astype('datetime64')





""" Price efficiency """
efficiency = df.copy()
print(efficiency)

# generate mid price
efficiency['mid'] = (efficiency['bid'] + efficiency['ask']) / 2
efficiency['date_daily'] = efficiency['date'].dt.date
efficiency = efficiency[['ric', 'mid', 'date_daily']]

# resample mid price by 5 minutes
efficiency = (efficiency.groupby(by=['ric', 'date_daily']).resample('5min').last()).dropna(subset=['mid'])
efficiency = efficiency[['mid']]
efficiency = efficiency.reset_index()

# generate squared returns by ric and date_daily
efficiency['ret'] = ((efficiency.groupby(by=['ric', 'date_daily'])['mid'].pct_change()))
efficiency = efficiency.drop(['mid'], axis=1).reset_index(drop=True).dropna(subset=['ret'])

efficiency.loc[efficiency['ret'] > 0.20, 'dummy'] = 1
efficiency['dummy'] = efficiency['dummy'].fillna(0)

drop = efficiency.groupby(by=['ric', 'date_daily']).sum().reset_index()
drop = drop[~(drop['dummy'] > 0)].reset_index()
drop = drop[['ric', 'date_daily']]

efficiency = pd.merge(drop, efficiency, on=['ric', 'date_daily'], how='left')
efficiency = efficiency.drop(columns=['dummy'])

# generate first oder autocorrelation for each stock-day combination
efficiency = pd.DataFrame(efficiency.groupby(['ric', 'date_daily'])['ret'].apply(pd.Series.autocorr, lag=1)).reset_index().rename(columns={'ret': 'price_efficiency'})
efficiency['price_efficiency'] = abs(efficiency['price_efficiency'])









# # # # # # # # # # # # # # # # # # # # # # # # #
""" 4. we create the market quality measures
which are generate with trade and quote data """
# # # # # # # # # # # # # # # # # # # # # # # # #

""" Liquidity """
# We use several variables to capture the three dimensions of liquidity as defined by Kyle (1985):
# tightness, depth, and resiliency.

""" Tightness = Quoted Spread """
# A standard measure of tightness is the quoted relative half-spread, which we compute as
# the weighted average across all time intervals on day t and denote by quoted spread
q_spread = df_quotes.copy()
# drop trades
q_spread = q_spread.loc[~(q_spread['type'] == 'Trade')]
q_spread = q_spread[['date_daily', 'ric', 'bid', 'ask', 'bid_size', 'ask_size']]

# generate quoted spread
q_spread['quoted_spread'] = (q_spread['ask'] - q_spread['bid']) / ((q_spread['bid'] + q_spread['ask']) / 2)

# generate weights calculate an weighted spread
# we weight by the volume outstanding at best bid and ask
q_spread['outstanding'] = (q_spread['bid_size'] + q_spread['ask_size'])
q_spread = q_spread[['date_daily', 'ric', 'outstanding', 'quoted_spread']]

# sum all the outstanding quoted volume
sum = (q_spread.groupby(by=['ric', 'date_daily'])['outstanding'].sum())
sum = sum.reset_index().rename(columns={'outstanding': 'total_outstanding'})
q_spread = pd.merge(q_spread, sum, on=['ric', 'date_daily'], how='left')
q_spread['weight'] = q_spread['outstanding'] / q_spread['total_outstanding']

# we weight the quoted spread with the weights
q_spread['quoted_spread'] = q_spread['weight'] * q_spread['quoted_spread']
q_spread = (q_spread.groupby(by=['ric', 'date_daily'])['quoted_spread'].sum()).reset_index()





""" Effective Spread """
eff_spread = df_quotes.copy()

# Lee–Ready algorithm:
# Inferring Trade Direction from Intraday Data CHARLES. C. LEE and MARK. READY
# A trade is assigned the sign +1 (−1), meaning that the trade was buyer
# initiated (seller initiated) if the transaction price is above (below) the mid-price.
eff_spread['mid'] = (eff_spread['bid'] + eff_spread['ask']) / 2
eff_spread = eff_spread[['date_daily', 'ric', 'mid', 'price']]

eff_spread.loc[eff_spread['price'] > eff_spread['mid'], 'trade_direction'] = 1
eff_spread.loc[eff_spread['price'] <= eff_spread['mid'], 'trade_direction'] = -1
eff_spread['effective_spread'] = eff_spread['trade_direction'] * ((eff_spread['price'] - eff_spread['mid']) / eff_spread['mid'])
eff_spread = eff_spread[['date_daily', 'ric', 'effective_spread']]
eff_spread = eff_spread.dropna(subset=['effective_spread'])
eff_spread = eff_spread.groupby(by=['ric', 'date_daily'])['effective_spread'].mean()
eff_spread = eff_spread.reset_index()





""" Price Impact """
impact = df_quotes.copy()

# mid price
impact['mid'] = (impact['bid'] + impact['ask']) / 2
impact = impact[['ric', 'price', 'mid']]

# resample to 1 second se we can get the five minutes
# buckets for the price impact and realized spread
impact = (impact.groupby(by=['ric']).resample('S').mean()).reset_index()

impact.loc[impact['price'] > impact['mid'], 'trade_direction'] = 1
impact.loc[impact['price'] <= impact['mid'], 'trade_direction'] = -1

# if there is no update in the LOB,
# the last best bid and best ask quote is still valid
impact['mid'] = impact['mid'].fillna(method='ffill')

# finds the five minutes ahead mid price
impact['mid_5'] = impact.groupby(by=['ric'])['mid'].shift(-300)

# generate the price impact measure
impact['price_impact'] = (impact['trade_direction'] * ((impact['mid_5'] - impact['mid']) / impact['mid']))
impact['realized_spread'] = (impact['trade_direction'] * ((impact['price'] - impact['mid_5']) / impact['mid']))
impact.dropna(subset=['price_impact'], inplace=True)
impact = impact[['ric', 'date', 'price_impact', 'realized_spread']]

# set date as datetime and as index
impact = impact.set_index(pd.DatetimeIndex(impact['date']))

# resample the data so we have daily measures for price_impact and realized_spread
impact = (impact.groupby(by=['ric']).resample('D').mean()).reset_index()

# drop weekends in time series
impact.dropna(subset=['realized_spread'], inplace=True)

# rename the date to date_daily so we can merge
impact.rename(columns={'date':'date_daily'}, inplace=True)





""" Depth """
depth = df_quotes.copy()

# generate market depth
depth['depth'] = depth['bid'] * depth['bid_size'] + depth['ask'] * depth['ask_size']
depth = depth[['ric', 'depth']]

# resample the data so we have daily measures for price_impact and realized_spread
depth = (depth.groupby(by=['ric']).resample('D').mean()).reset_index()
depth['depth'] = depth['depth'] / 1000

# drop weekends in time series
depth.dropna(subset=['depth'], inplace=True)

# rename the date to date_daily so we can merge
depth.rename(columns={'date':'date_daily'}, inplace=True)









# merge all the data
df = df[['ric', 'date_daily']]
df = df.reset_index().drop(['date'], axis=1)
df = df.drop_duplicates().reset_index().drop(['index'], axis=1)

df = pd.merge(df, activity, on=['ric', 'date_daily'], how='left')
df = pd.merge(df, real_vol, on=['ric', 'date_daily'], how='left')
df = pd.merge(df, range, on=['ric', 'date_daily'], how='left')
df = pd.merge(df, efficiency, on=['ric', 'date_daily'], how='left')
df = pd.merge(df, q_spread, on=['ric', 'date_daily'], how='left')
df = pd.merge(df, eff_spread, on=['ric', 'date_daily'], how='left')
df = pd.merge(df, impact, on=['ric', 'date_daily'], how='left')
df = pd.merge(df, depth, on=['ric', 'date_daily'], how='left')

print(df)
print(df.describe())


df.to_excel('H:/data/Liquiditymeasures/Stocks/Liquiditymeasures_Stocks_Francenew.xlsx')

print("--- %s seconds ---" % (time.time() - start_time))