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
    print('login respond error_code:'+lg.error_code, file=sys.stderr)
    print('login respond  error_msg:'+lg.error_msg, file=sys.stderr)

#### 获取沪深A股估值指标(日频)数据 ####
# peTTM    滚动市盈率
# psTTM    滚动市销率
# pcfNcfTTM    滚动市现率
# pbMRQ    市净率
rs = bs.query_history_k_data_plus(code,
    "date,close,peTTM,pbMRQ,psTTM,pcfNcfTTM",
    start_date='2000-01-01', end_date=str(datetime.date.today()),
    frequency="d", adjustflag="3")
if rs.error_code != "0":
    print('query_history_k_data_plus respond error_code:'+rs.error_code, file=sys.stderr)
    print('query_history_k_data_plus respond  error_msg:'+rs.error_msg, file=sys.stderr)

#### 打印结果集 ####
result_list = []
while (rs.error_code == '0') & rs.next():
    # 获取一条记录，将记录合并在一起
    result_list.append(rs.get_row_data())
result = pd.DataFrame(result_list, columns=rs.fields)

#### 结果集输出到csv文件 ####
f = open("%s/%s.ttm"%(output_dir, code), "w")
f.write(result.to_string(index=False))
f.close()

#### 登出系统 ####
bs.logout()
