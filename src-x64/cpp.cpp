#include <iostream>
#include <string>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]

CharacterVector parseString(CharacterVector x) {
  int timezone_offset = 0;
  int absolute_date = 0;
  int date = 0;
  string row = "";
  int pos = 0;
  string temp = "";

  for (int i = 6; i < x.size(); i++)
  {
    row = string(x[i]);

    // if equals to "TIMEZONE_OFFSET=..."
    if(row.substr(0,1) == "T"){
      timezone_offset = atoi(row.substr(16).c_str());
      x.erase(i);
      i--;
    }
    else{
      pos = row.find(",");
      temp = row.substr(0, pos);
      // if equals to "a..."
      if(temp.substr(0,1) == "a"){
        absolute_date = atoi(temp.substr(1).c_str());
        date = absolute_date;
      }
      else{
        date = absolute_date + atoi(temp.c_str());
      }

      temp = row.substr(pos + 1);
      stringstream convert1, convert2;
      convert1 << date;
      convert2 << timezone_offset;
      x[i] = convert1.str() + "," + convert2.str() + "," + temp;


    }
  }
  x.erase(0, 6);

  return x;
}


/*** R
parseString(c("EXCHANGE%3DNASDAQ",
              "MARKET_OPEN_MINUTE=570",
              "MARKET_CLOSE_MINUTE=960",
              "INTERVAL=86400",
              "COLUMNS=DATE,CLOSE,HIGH,LOW,OPEN,VOLUME,CDAYS",
              "DATA=",
              "TIMEZONE_OFFSET=-240",
              "a1156795200,9.57,9.8,9.53,9.79,184557695,1",
              "1,9.5,9.61,9.3,9.57,236878929,1",
              "2,9.566,9.689,9.526,9.619,170067758,1",
              "TIMEZONE_OFFSET=60",
              "3,9.69,9.76,9.52,9.61,143701096,1",
              "4,9.77,9.81,9.69,9.78,102156376,1",
              "8,10.21,10.21,9.79,9.85,253297520,1",
              "TIMEZONE_OFFSET=0",
              "9,10,10.24,9.96,10.15,243554661,1",
              "10,10.4,10.5,10.04,10.09,317006165,1"))
*/
