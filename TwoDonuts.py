import dash
from datetime import datetime as dt
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import pandas as pd
from toolz import compose, pluck, groupby, valmap, first, unique, get, countby

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__)

df = pd.read_csv('MasterFile.csv')
df['Date']=pd.to_datetime(df['Date'], format='%d-%m-%Y')
available_indicators=df['Segment'].unique()
app.layout =html.Div([
    html.Div([
            dcc.Dropdown(
                id='xaxis-column',
                options=[{'label': i, 'value': i} for i in available_indicators],
                value='Cash'
            )],style={'width': '49%', 'float': 'right', 'display': 'inline-block'}),
    html.Div([
            
            dcc.DatePickerRange(
                id='date-picker-range',
                start_date_placeholder_text='Select a starting date',
                end_date_placeholder_text='Select an ending date',
                start_date=min(df['Date']),
                end_date=max(df['Date'])
            )],style={'width': '49%', 'float': 'left', 'display': 'inline-block'}),
            
    html.Div([        
            dcc.Input(id="initialload", type='hidden'),
            dcc.Graph(id = 'sequencepiechart')], style={'width': '49%', 'float': 'right', 'display': 'inline-block'}),
    html.Div([        
            dcc.Graph(id = 'sequencepiechart2')], style={'width': '49%', 'float': 'left', 'display': 'inline-block'})
        ])



#Callbacks for Pie Chart

@app.callback(
  Output(component_id = "sequencepiechart", component_property = "figure"),
  [Input('xaxis-column', 'value'),
  Input(component_id="initialload", component_property="value"),
  Input(component_id='date-picker-range', component_property='start_date'),
  Input(component_id='date-picker-range', component_property='end_date')])
def sequencepiechart(xaxis_column_name,value,start_date,end_date):
    # print(a['Outcome'])
    # print(a['Counts'])
    dff=df[(df['Date'] >= start_date) & (df['Date'] <= end_date)]
    b=dff[dff['Status'] == 3].groupby(['Segment','Outcome'])['Date'].count()
    b=b.reset_index()
    b.columns=['Segments','Outcome','Counts']
    bb = b[b['Segments'] == xaxis_column_name]
    return {
        "data": [
            {
                "type": "pie",
                "labels": bb['Outcome'],
                "values": bb['Counts'],
                "hole": 0.4
            }
        ],
        "layout": {
            "title": "Outcomes"
        }
    }
  

@app.callback(
  Output(component_id = "sequencepiechart2", component_property = "figure"),
  [Input('xaxis-column', 'value'),
  Input(component_id="initialload", component_property="value"),
  Input(component_id='date-picker-range', component_property='start_date'),
  Input(component_id='date-picker-range', component_property='end_date')])
def sequencepiechart2(xaxis_column_name,value,start_date,end_date):
    # print(a['Outcome'])
    # print(a['Counts'])
    dff1=df[(df['Date'] >= start_date) & (df['Date'] <= end_date)]
    b1=dff1[dff1['Status'] == 3].groupby(['Segment','PnL'])['Date'].count()
    b1=b1.reset_index()
    b1.columns=['Segments','PnL','Counts']
    bb1 = b1[b1['Segments'] == xaxis_column_name]
    return {
        "data": [
            {
                "type": "pie",
                "labels": bb1['PnL'],
                "values": bb1['Counts'],
                "hole": 0.4
            }
        ],
        "layout": {
            "title": "Profit & Loss"
        }
    } 

if __name__ == '__main__':
    app.run_server(debug=True)
