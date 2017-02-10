import pandas as pd
import numpy as np

import statsmodels.formula.api as smf
import sys
from natsort import natsorted

import matplotlib.pyplot as plt
import seaborn as sns

#artifacts
first_art_begin = 0.081 
first_art_end = 0.0845 
second_art_begin = 0.131
second_art_end = 0.134

#time points of traces for analysis
end_cap = 0.02
first_stim_begin = 0.08
first_stim_end = 0.125
second_stim_begin = 0.136
second_stim_end = 0.175

#time points for finding NMDA decay time
end_tau_trace = 0.58
begin_NMDA_tau = 0.115
end_NMDA_tau = 0.5

#for finding tau of capacitance traces
begin_cap_curve = 0.00565
end_cap_curve = 0.011
begin_cap_baseline = 0.010
tau_threshold = 0.001

def select_waves(df, wave_info):
    '''Takes the data frame of raw igor traces and: 
    1) indexes waves by wave names (info from excel file)
    2) removes traces with NaN values'''
    igor_waves = df.pivot(index = 'sec', columns = 'id', values = 'pA')
    waves = igor_waves.filter(wave_info['waveName'])
    waves = waves.dropna(axis = 0)
    return waves

def normalize_traces(df, begin, end):
    '''Normalizes traces to baseline'''
    baseline = df[begin:end]
    baseline = baseline.dropna(axis = 0)
    avg_baseline = baseline.mean()
    normalized_traces = df.subtract(avg_baseline)
    return normalized_traces

def remove_artifact(df, begin1, end1, begin2 = None, end2 = None):
    '''Removes stimulus artifact(s)'''
    df[begin1:end1] = np.nan
    df[begin2:end2] = np.nan
    return df

def process_waves(df, begin1 = first_art_begin, end1 = first_art_end, 
                  begin2 = second_art_begin, end2 = second_art_end, 
                  begin_norm = end_cap, end_norm = first_art_begin):
    '''Remove stimulus artifact(s), normalize traces to baseline
    begin1/end1: first stimulus artifact
    begin2/end2: second stimulus artifact
    begin_norm, end_norm: baseline'''
    df1 = remove_artifact(df, begin1, end1, begin2, end2)
    df2 = normalize_traces(df1, begin_norm, end_norm)
    return df2

def divide_into_sections(df, begin1 = first_stim_begin, end1 = first_stim_end, 
                         begin2 = second_stim_begin, end2 = second_stim_end):
    '''Divide waves into sections for analysis'''
    first = df[begin1:end1]
    second = df[begin2:end2]
    both = df[begin1:end2]
    return first, second, both

def smooth(df, window = 150):
    '''Uses rolling average window to smooth curve'''
    smoothed = df.rolling(window).mean()
    return smoothed

def filter_waves(df, *args, wave_info, keep = True):
    '''Filter waves by information in notes column of wave_info data frame;
    keep is true by default'''
    notes = wave_info.loc[:, ['waveName', 'notes']]
    waves_index = []
    for note in args:
        #selects wave ids (waveName) that matches args
        waves = list(notes[(notes['notes'] == note)]['waveName'])
        waves_index.append(waves)
        
    if len(waves) == 0:
        if note == 'noBic':
            print('inhibitory waves not present (bicuculline present)')
        if note == '2F':
            print('no secondary fibers')
        if note == 'lastMax':
            print('no final maximals')
        if note == '2nd':
            pass
        if note == 'SF':
            print('no single fibers')
        
    final_waves = [item for sublist in waves_index for item in sublist]
    
    if keep:
        df = df[final_waves]
    elif keep == False: 
        df = df.drop(final_waves, 1)
    
    return df

def plot_all(df, wave_info, date, age, cell_num):
    '''
    Plot all waves.
    Arguments: df of waves, date of cell, cell number, age of mouse
    '''
    
    #remove noBic, test and NMDA tau waves
    df1 = filter_waves(df, 'noBic', 'test', 'NMDA tau', wave_info = wave_info, keep = False)
    if df1.empty:
        pass
    else:
        sns.axes_style('darkgrid')
        sns.set_palette('husl')
        fig, ax = plt.subplots()
        fig.set_size_inches(11.7, 8.27)

        _ = plt.plot(df1, linewidth = 0.5)
        _ = plt.xlabel('seconds')
        _ = plt.ylabel('pA')
        _ = plt.title(date + ': ' + age + ' cell ' + str(cell_num))


def calculate_maximals(df, second, wave_info):
    '''Calculate maximals. Because of noise, amplitude of failure traces must be averaged.
    Second = False, finds failures in first stim period.
    Returns df_of_maximals, failures_amp.index'''
    df_of_maximals = pd.DataFrame(abs(df).max(), columns = ['maximals'])
    
    #Find maximals of failures in first stimulation period
    if second == False:
        failures = filter_waves(df, 'x', '2nd', wave_info = wave_info)
    
    #Find maximals of failures in second stimulation period
    elif second:
        failures = filter_waves(df, 'x', wave_info = wave_info)
    
    failures_amp = pd.DataFrame(failures.mean(), columns = ['maximals'])
    
    #Replace values in df_of_maximals with values in failures_amp
    df_of_maximals.ix[failures_amp.index] = abs(failures_amp)
    
    return df_of_maximals, failures_amp.index

def split_rows_AN(df):
    '''Split df where rows are waves into AMPA/NMDA,
    returns AMPA and NMDA'''
    AMPA = df[::2]
    NMDA = df[1::2]
    return AMPA, NMDA

def recombine_AN(dfA, dfN):
    '''Recombines individual AMPA and NMDA dfs
    back into one df in an ordered manner'''
    temp = list(zip(dfA, dfN))
    recombined = [item for sublist in temp for item in sublist]
    return recombined

def find_maxAN_toplot(df_first, df_both, wave_info, second = False):
    '''
    Finds the max AMPA and max NMDA waves for plotting.
    '''
    all_max, _ = calculate_maximals(df_first, second, wave_info)
    maxA, maxN = split_rows_AN(all_max)
    
    #identify max AMPA and NMDA waves
    maxA = df_first[maxA.idxmax()]
    maxN = df_first[maxN.idxmax()]
    
    #create df of merged AMPA and NMDA maximal waves
    max_AN = df_both.filter(recombine_AN(maxA, maxN))
    return max_AN

def plot_max_SF(df_first, df_both, wave_info, date, age, cell_num):
    '''
    Plots maximals, single fibers and last maximals.
    Arguments are df of waves in first stimulation paradigm and
    df of waves in both stimulation paradigms.
    '''
    #waves to plot
    maxes = find_maxAN_toplot(df_first, df_both, wave_info)
    SFs = filter_waves(df_both, 'SF', wave_info = wave_info)
    last_max = filter_waves(df_both, 'lastMax', wave_info = wave_info)
    
    #set figure style
    sns.axes_style('darkgrid')
    fig, ax = plt.subplots()
    fig.set_size_inches(11.7, 8.27)
    
    #plots
    _ = plt.plot(maxes, color = 'r', label = 'maximals', linewidth = 0.5)
    if SFs.empty:
        pass
    else:
        _ = plt.plot(SFs, color = 'k', label = 'SF', linewidth = 0.5)
    if last_max.empty:
        pass
    else:
        _ = plt.plot(last_max, color = 'b', label = 'final waves', linewidth = 0.5) 
    
    #plot attributes
    _ = plt.legend(loc = 4, fontsize = 'x-small', ncol = 3)
    _ = plt.title(date + ': ' + age + ' cell ' + str(cell_num) + ' SFs and maximals')
    _ = plt.xlabel('seconds')
    _ = plt.ylabel('pA')

def plot_SF(df_both, wave_info, date, age, cell_num):
    SFs = filter_waves(df_both, 'SF', wave_info = wave_info)
    SiFs = filter_waves(df_both, 'SiF', wave_info = wave_info)
    if SFs.empty & SiFs.empty:
        pass
    else:
        if SFs.empty:
            pass
        else:
            #set figure style
            sns.axes_style('darkgrid')
            fig, ax = plt.subplots()
            fig.set_size_inches(11.7, 8.27)
            _ = plt.plot(SFs, color = 'k', label = 'SF', linewidth = 0.5)

            #plot attributes
            _ = plt.legend(loc = 4, fontsize = 'x-small', ncol = 3)
            _ = plt.title(date + ': ' + age + ' cell ' + str(cell_num) + ' single fibers')
            _ = plt.xlabel('seconds')
            _ = plt.ylabel('pA')

        if SiFs.empty:
            pass
        else:
            #avg SiF
            SiF_mean = pd.DataFrame()
            SiF_mean['AMPA'] = SiFs[SiFs.columns[::2]].mean(axis = 1)
            SiF_mean['NMDA'] = SiFs[SiFs.columns[1::2]].mean(axis = 1)
            _ = plt.plot(SiFs, color = '0.5', label = 'SiFs', linewidth = 0.2)
            _ = plt.plot(SiF_mean, color = 'm', label = 'SiF mean', linewidth = 0.5)


def find_cap_wave(df, begin = 0, end = end_cap):
    return df[begin:end]

def plot_inhibitory(df, wave_info, date, age, cell_num):
    inh = filter_waves(df, 'noBic', wave_info = wave_info)
    if len(inh.columns) == 0:
        pass
    else:
        sns.axes_style('darkgrid')
        sns.set_palette('husl')
        fig, ax = plt.subplots()
        fig.set_size_inches(11.7, 8.27)

        _ = plt.xlabel('seconds')
        _ = plt.ylabel('pA')
        _ = plt.plot(inh, linewidth = 0.5)
        _ = plt.title(date + ': ' + age + ' cell ' + str(cell_num) + ' inhibitory traces (no bicuculline)')

def plot_indiv_waves(df, wave_info, *args):
    '''Plots individual waves by wave id for basic visualization'''
    to_select = []
    for id in args:
        wave = wave_info[wave_info['waveName'].values == id]['waveName']
        to_select.append(wave)
    selection = [item for sublist in to_select for item in sublist]
    return plt.plot(df[selection])



