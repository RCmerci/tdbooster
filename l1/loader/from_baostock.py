# -*- coding: utf-8 -*-

import baostock as bs
import pandas as pd
import datetime
import sys
code = sys.argv[1]
output_dir = sys.argv[2]

#### 登陆系统 ####
lg = bs.login()
# 显示登陆返回信息
if lg.error_code != "0":
    print( 'login respond error_code:'+lg.error_code, file=sys.stderr)
    print( 'login respond  error_msg:'+lg.error_msg, file=sys.stderr)

#### 获取沪深A股历史K线数据 ####
# 分钟线指标：date,time,code,open,high,low,close,volume,amount,adjustflag
for i in (1,2,3):
    rs = bs.query_history_k_data_plus(code,
                                      "date,open,high,low,close,pctChg,preclose,volume,amount,adjustflag,turn,tradestatus,isST",
                                      start_date='2010-01-01', end_date=str(datetime.date.today()),
                                      frequency="d", adjustflag="2")
    if rs.error_code != '0':
        if rs.error_msg == '用户未登录':
            bs.login()
        else:
            print('query_history_k_data_plus,'+code+',errcode:'+rs.error_code, file=sys.stderr)
            print('query_history_k_data_plus,'+code+',errmsg:'+rs.error_msg, file=sys.stderr)
    elif rs.next():
        break

#### 打印结果集 ####
data_list = []
while (rs.error_code == '0') & rs.next():
    # 获取一条记录，将记录合并在一起
    data_list.append(rs.get_row_data())
result = pd.DataFrame(data_list, columns=rs.fields)

#### 结果集输出到文件 ####
f = open("%s/%s"%(output_dir, code), "w")
f.write(result.to_string(index=False))
f.close()

#### 登出系统 ####
bs.logout()
