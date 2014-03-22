data(lab.daily)

my.syndromic <- raw_to_syndromic (id=SubmissionID, 
                                  syndromes.var=Syndrome,
                                  dates.var=DateofSubmission, 
                                  date.format="%d/%m/%Y", 
                                  remove.dow=c(6,0),
                                  add.to=c(2,1),
                                  sort=TRUE,
                                  data=lab.daily)

my.syndromic <- holt_winters_synd(x=my.syndromic,
                                  evaluate.window=30,
                                  frequency=5,
                                  baseline.window=260,
                                  limit.sd=c(2.5,3,3.5), #default
                                  nahead=5,
                                  correct.baseline=2,
                                  alarm.dim=1)


my.syndromic <- ewma_synd(x=my.syndromic,
                          syndromes="Musculoskeletal",
                          evaluate.window=30,
                          baseline.window=260,
                          lambda=0.2,
                          limit.sd=c(2.5,3,3.5),
                          guard.band=5,
                          correct.baseline=FALSE,
                          alarm.dim=2,
                          UCL=2,
                          LCL=FALSE,                          
                          pre.process="glm",
                          family="poisson",
                          formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
                          period=260)


my.syndromic <- ewma_synd(x=my.syndromic,
                          syndromes= c(1,2,4,5),
                          evaluate.window=30,
                          baseline.window=260,
                          lambda=0.2,
                          limit.sd=c(2.5,3,3.5),
                          guard.band=5,
                          UCL=2,
                          correct.baseline=FALSE,
                          alarm.dim=2,
                          pre.process="diff",
                          diff.window=5)


###############################
getwd()

syndromic_alarm(x=my.syndromic,
              date=NULL,
              plot.all=FALSE,
              window=365,
              baseline=TRUE,
               UCL=1,
               algorithms=NULL,
               limit=1,
               height=7.5,
               width=10.5,
               pdf.dir=TRUE,
               file.name=NULL)

getwd()

syndromic_alarm(x=my.syndromic,
              date=NULL,
              plot.all=TRUE,
              window=365,
              baseline=TRUE,
              UCL=1,
              algorithms=NULL,
              limit=1,
              height=7.5,
              width=10.5,
              pdf.dir=TRUE,
              file.name=NULL)

getwd()

syndromic_alarm(x=my.syndromic,
              date="2013-01-01",
              plot.all=TRUE,
              window=365,
              baseline=TRUE,
              UCL=1,
              algorithms=NULL,
              limit=1,
              height=7.5,
              width=10.5,
              pdf.dir=TRUE,
              file.name=NULL)

getwd()

syndromic_alarm(x=my.syndromic,
              date=NULL,
              plot.all=TRUE,
              window=200,
              baseline=TRUE,
              UCL=1,
              algorithms=NULL,
              limit=1,
              height=7.5,
              width=10.5,
              pdf.dir=TRUE,
              file.name="window200")

getwd()


syndromic_alarm(x=my.syndromic,
              date=NULL,
              plot.all=TRUE,
              window=365,
              baseline=FALSE,
              UCL=1,
              algorithms=NULL,
              limit=1,
              height=7.5,
              width=10.5,
              pdf.dir=TRUE,
              file.name="test3")

getwd()

syndromic_alarm(x=my.syndromic,
              date=NULL,
              plot.all=TRUE,
              window=365,
              baseline=TRUE,
              UCL=2,
              algorithms=NULL,
              limit=1,
              height=7.5,
              width=10.5,
              pdf.dir=TRUE,
              file.name="test4")

getwd()

syndromic_alarm(x=my.syndromic,
              date=NULL,
              plot.all=TRUE,
              window=365,
              baseline=TRUE,
              UCL=1,
              algorithms=1,
              limit=1,
              height=7.5,
              width=10.5,
              pdf.dir=TRUE,
              file.name="test5")

getwd()


syndromic_alarm(x=my.syndromic,
              date=NULL,
              plot.all=TRUE,
              window=365,
              baseline=TRUE,
              UCL=1,
              algorithms=NULL,
              limit=3,
              height=7.5,
              width=10.5,
              pdf.dir=TRUE,
              file.name="test6")

getwd()


syndromic_alarm(x=my.syndromic,
              date=NULL,
              plot.all=TRUE,
              window=365,
              baseline=TRUE,
              UCL=1,
              algorithms=NULL,
              limit=1,
              height=7.5,
              width=10.5,
              pdf.dir=FALSE,
              file.name="test7")

getwd()



syndromic_alarm (x=my.syndromic,
          pdf_report=TRUE,
          email_alarm_to="<dorea.meyer@gmail.com>",
          email_noalarm_to="<dorea.meyer@gmail.com>",
          date=NULL,
          plot.all=TRUE,
          window=365,
          baseline=TRUE,
          UCL=1,
          algorithms=NULL,
          limit=1,
          email_from=NULL,
          smtpServer=NULL,
          subject= paste((Sys.Date()),"ALARM: There are alarms today",sep=","),
          message=NULL,
          height=7.5,
          width=10.5,
          pdf.dir=TRUE,
          file.name="test11")



syndromic_alarm (x=my.syndromic,
                 pdf_report=TRUE,
                 email_alarm_to="<dorea.meyer@gmail.com>",
                 email_noalarm_to="<dorea.meyer@gmail.com>",
                 date=NULL,
                 plot.all=TRUE,
                 window=365,
                 baseline=TRUE,
                 UCL=1,
                 algorithms=NULL,
                 limit=1,
                 email_from="<fernanda.dorea@sva.se>",
                 smtpServer="smtp1.sva.se",
                 subject= paste((Sys.Date()),"ALARM: There are alarms today",sep=","),
                 message=NULL,
                 height=7.5,
                 width=10.5,
                 pdf.dir=TRUE,
                 file.name="test12")



syndromic_alarm (x=my.syndromic,
                 pdf_report=TRUE,
                 email_alarm_to="<dorea.meyer@gmail.com>",
                 email_noalarm_to="<dorea.meyer@gmail.com>",
                 date=NULL,
                 plot.all=FALSE,
                 window=365,
                 baseline=TRUE,
                 UCL=1,
                 algorithms=NULL,
                 limit=1,
                 email_from="<fernanda.dorea@sva.se>",
                 smtpServer="smtp1.sva.se",
                 subject= paste((Sys.Date()),"ALARM: There are alarms today",sep=","),
                 message=NULL,
                 height=7.5,
                 width=10.5,
                 pdf.dir=TRUE,
                 file.name="test12")


syndromic_alarm (x=my.syndromic,
                 pdf_report=TRUE,
                 email_alarm_to="<dorea.meyer@gmail.com>",
                 email_noalarm_to="<dorea.meyer@gmail.com>",
                 date=NULL,
                 plot.all=FALSE,
                 window=365,
                 baseline=TRUE,
                 UCL=1,
                 algorithms=NULL,
                 limit=1,
                 email_from="<fernanda.dorea@sva.se>",
                 smtpServer="smtp1.sva.se",
                 subject= paste((Sys.Date()),"ALARM: There are alarms today",sep=","),
                 message=NULL,
                 height=7.5,
                 width=10.5,
                 pdf.dir=TRUE,
                 file.name="test12")


syndromic_alarm (x=my.syndromic,
                 pdf_report=TRUE,
                 email_alarm_to="<dorea.meyer@gmail.com>",
                 email_noalarm_to="<dorea.meyer@gmail.com>",
                 date=NULL,
                 plot.all=FALSE,
                 window=365,
                 baseline=TRUE,
                 UCL=1,
                 algorithms=NULL,
                 limit=1,
                 email_from="<fernanda.dorea@sva.se>",
                 smtpServer="smtp1.sva.se",
                 message=NULL,
                 height=7.5,
                 width=10.5,
                 pdf.dir=TRUE,
                 file.name="test12")