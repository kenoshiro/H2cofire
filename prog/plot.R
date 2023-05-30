mytheme <- list()
mytheme$set1 <- theme_bw()+theme(plot.title=element_text(size=9.5),
                                 panel.grid=element_blank(),
                                 panel.border=element_blank(),
                                 axis.line=element_line(color='black',linewidth=.3),
                                 axis.ticks=element_line(color='black',size=.3),
                                 axis.text=element_text(size=9),
                                 axis.title=element_text(size=10),
                                 legend.title=element_text(size=10))

df$scen_lab <- tribble(~Scenario,~scen_lab,~scen_wrap,
                       '500_Default','500_Default','500_Default',
                       '500_LimCCS','500_LimCCS','500_LimCCS',
                       '500_H2Opt','500_H2Opt','500_H2Opt',
                       '500_NoCOF','500_NoCOF','500_NoCOF',
                       '700_Default','700_Default','700_Default',
                       '700_LimCCS','700_LimCCS','700_LimCCS',
                       '700_H2Opt','700_H2Opt','700_H2Opt',
                       '700_NoCOF','700_NoCOF','700_NoCOF',
                       '1000_Default','1000_Default','1000_Default',
                       '1000_LimCCS','1000_LimCCS','1000_LimCCS',
                       '1000_H2Opt','1000_H2Opt','1000_H2Opt',
                       '1000_NoCOF','1000_NoCOF','1000_NoCOF',
                       '1400_Default','1400_Default','1400_Default',
                       '1400_LimCCS','1400_LimCCS','1400_LimCCS',
                       '1400_H2Opt','1400_H2Opt','1400_H2Opt',
                       '1400_NoCOF','1400_NoCOF','1400_NoCOF')

df$scen_cat <- tribble(~Scenario,~scen_cpol,~scen_techpol,
                       '500_Default','500','Default',
                       '500_LimCCS','500','LimCCS',
                       '500_H2Opt','500','H2Opt',
                       '500_NoCOF','500','NoCOF',
                       '700_Default','700','Default',
                       '700_LimCCS','700','LimCCS',
                       '700_H2Opt','700','H2Opt',
                       '700_NoCOF','700','NoCOF',
                       '1000_Default','1000','Default',
                       '1000_LimCCS','1000','LimCCS',
                       '1000_H2Opt','1000','H2Opt',
                       '1000_NoCOF','1000','NoCOF',
                       '1400_Default','1400','Default',
                       '1400_LimCCS','1400','LimCCS',
                       '1400_H2Opt','1400','H2Opt',
                       '1400_NoCOF','1400','NoCOF')
lst$scen_techpol <- c('Default','H2Opt','LimCCS','NoCOF')
lst$scen_cpol <- c('500','700','1000','1400')
lst$R5 <- c('R5OECD90+EU','R5REF','R5ASIA','R5MAF','R5LAM')

lst$color_cpol <- c('500'='salmon','700'='lightskyblue3','1000'='darkolivegreen2','1400'='lightgoldenrod3')
lst$color_tech <- c('Default'='#E64B35FF','H2Opt'='#3C5488FF','LimCCS'='#00A087FF','NoCOF'='#F39B7FFF')
lst$shape_tech <- c('Default'=21,'H2Opt'=22,'LimCCS'=23,'BAThigh'=24,'NoCOF'=25)


# Fig.1 -----------------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Ele_Oil','Oil','sandybrown',
                  'Sec_Ene_Ele_Coa','Coal','grey50',
                  'Sec_Ene_Ele_Gas','Gas','lightgoldenrod',
                  'Sec_Ene_Ele_Nuc','Nuclear','moccasin',
                  'Sec_Ene_Ele_Hyd','Hydro','lightsteelblue',
                  'Sec_Ene_Ele_Bio','Biomass','darkolivegreen2',
                  'Sec_Ene_Ele_Geo','Geothermal','peru',
                  'Sec_Ene_Ele_SolarCSP','CSP','darksalmon',
                  'Sec_Ene_Ele_SolarPV','Solar PV','lightsalmon',
                  'Sec_Ene_Ele_Win','Wind','lightskyblue3',
                  'Sec_Ene_Ele_Oce','Ocean','paleturquoise3',
                  'Sec_Ene_Ele_Oth','Other','grey')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$sec_ene_ele <- df$all %>% filter(Region=='World',Year%in%seq(2020,2050,5)) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    filter(scen_techpol=='Default') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),
           scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    ggplot()+
    geom_area(aes(x=Year,y=Value,fill=Variable),position='stack',show.legend=T)+
    facet_grid(.~scen_cpol)+
    labs(x=NULL,y=expression(paste('Power generation (EJ ',{yr}^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    guides(fill=guide_legend(title=NULL))

p$sec_ene_ele_fos <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable=='Sec_Ene_Ele_Fos') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=scen_cpol,group=interaction(scen_cpol,scen_techpol)),linewidth=.3,show.legend=T)+
    geom_point(aes(x=Year,y=Value,color=scen_cpol,shape=scen_techpol),fill='white',show.legend=T)+
    ylim(0,NA)+
    labs(x=NULL,y=expression(paste('Power generation (EJ ',{yr}^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),
                       legend.margin=margin(0,0,0,0))+
    scale_color_manual(values=lst$color_cpol)+
    scale_shape_manual(values=lst$shape_tech)+
    guides(color=guide_legend(title=NULL,override.aes=list(linetype=NA)),
           shape=guide_legend(title=NULL,override.aes=list(linetype=NA)))

p$share_cof <- df$all %>% filter(Region=='World',Year%in%c(2030,2050)) %>% 
    filter(Variable=='Sec_Ene_Ele_Share_Hyd_Cof_all') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    ggplot()+
    geom_line(aes(x=scen_cpol,y=Value,color=scen_techpol,group=scen_techpol))+
    geom_point(aes(x=scen_cpol,y=Value,color=scen_techpol,fill=scen_techpol,shape=scen_techpol),show.legend=T)+
    facet_grid(.~Year)+
    scale_y_continuous(limits=c(0,NA),labels=scales::percent_format(accuracy=.1))+
    labs(x=NULL,y='Share of hydrogen co-firing\nin total generation')+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_color_manual(values=lst$color_tech)+
    scale_fill_manual(values=lst$color_tech)+
    scale_shape_manual(values=lst$shape_tech)+
    guides(color=guide_legend(title=NULL),fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))

df$var <- tribble(~Variable,~Label,
                  'Prc_Car','var_x',
                  'Sec_Ene_Ele_Share_Hyd_Cof','var_y')
p$cof_carpri <- df$all %>% filter(Region=='World') %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    filter(scen_techpol!='NoCOF') %>% 
    select(-Unit,-Variable) %>% 
    pivot_wider(names_from='Label',values_from='Value') %>% 
    filter(var_x>0) %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    ggplot()+
    geom_point(aes(x=var_x,y=var_y,color=scen_cpol,shape=scen_techpol),show.legend=T)+
    xlim(0,NA)+
    scale_y_continuous(limits=c(0,1),labels=scales::percent_format(accuracy=1))+
    labs(x=expression(paste('Carbon prices (US$ ','t-',{CO[2]}^{-1},')')),y='Share of hydrogen co-firing\ninthermal generation')+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),
                       legend.margin=margin(0,0,0,0))+
    scale_color_manual(values=lst$color_cpol)+
    scale_shape_manual(values=lst$shape_tech)+
    guides(color=guide_legend(title=NULL),shape=guide_legend(title=NULL))

p$l_tmp <- get_legend(p$sec_ene_ele_fos)
p$tmp1 <- plot_grid(p$sec_ene_ele,p$sec_ene_ele_fos+theme(legend.position='none'),
                    nrow=1,rel_widths=c(1,.65),labels=c('a','b'))
p$tmp2 <- plot_grid(p$share_cof,p$cof_carpri+theme(legend.position='none'),
                    nrow=1,rel_widths=c(1,.8),labels=c('c','d'))
p$tmp3 <- plot_grid(p$tmp1,p$tmp2,ncol=1,rel_heights=c(1,1),labels=c('',''))
p$tmp <- plot_grid(p$tmp3,p$l_tmp,nrow=1,rel_widths=c(1,.15),labels=c('',''))
ggsave(filename='output/fig1.png',plot=p$tmp,width=180,height=130,units='mm',dpi=300)


# Fig.2 -------------------------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Cap_Ele_Coa_Una','Coal unabated','grey70',
                  'Cap_Ele_Coa_Hyd_Cof','Coal co-firing','grey50',
                  'Cap_Ele_Coa_w_CCS','Coal w/CCS','grey20',
                  'Cap_Ele_Gas_Una','Gas unabated','lightgoldenrod1',
                  'Cap_Ele_Gas_Hyd_Cof','Gas co-firing','lightgoldenrod3',
                  'Cap_Ele_Gas_w_CCS','Gas w/CCS','lightgoldenrod4')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$cap_ele_fos <- df$all %>% filter(Region=='World',Year%in%seq(2020,2050,5)) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    filter(scen_cpol%in%c('500','1000'),scen_techpol=='H2Opt') %>%
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),
           scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    ggplot()+
    geom_bar(aes(x=Year,y=Value,fill=Variable),stat='identity',position='stack',show.legend=T)+
    facet_grid(.~scen_cpol)+
    labs(x=NULL,y='Capacity (GW)')+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    guides(fill=guide_legend(title=NULL))

df$var <- tribble(~Variable,~Shape,
                  'Cap_Ele_Coa_and_Gas_Hyd_Cof','with co-firing',
                  'Cap_Ele_Fos_and_Hyd','Total')
lst$shp <- c('Total'=21,'with co-firing'=16)
p$cap_ele_hyd_cof <- df$all %>% filter(Region=='World',Year%in%c(2030,2050)) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol),
           Shape=factor(Shape,levels=names(lst$shp))) %>% 
    ggplot()+
    geom_point(aes(x=scen_cpol,y=Value,color=scen_techpol,shape=Shape),show.legend=T)+
    facet_grid(.~Year)+
    labs(x=NULL,y='Capacity (GW)')+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_shape_manual(values=lst$shp,name=NULL)+
    scale_color_manual(values=lst$color_tech,name=NULL)

df$var <- tribble(~Variable,~Legend,~Color,
                  'Cap_Fac_Ele_Coa_wo_CCS','Coal\nunabated','grey10',
                  'Cap_Fac_Ele_Coa_w_CCS','Coal\nwith CCS','grey30',
                  'Cap_Fac_Ele_Coa_Hyd_Cof','Coal\nwith co-firing','grey50',
                  'Cap_Fac_Ele_Gas_wo_CCS','Gas\nunabated','lightgoldenrod4',
                  'Cap_Fac_Ele_Gas_w_CCS','Gas\nwith CCS','lightgoldenrod3',
                  'Cap_Fac_Ele_Gas_Hyd_Cof','Gas\nwith co-firing','lightgoldenrod')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$cap_fac <- df$all %>% filter(Region%in%c('World'),Year%in%c(2030,2050)) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(Variable=factor(Variable,levels=df$var$Variable),Legend=factor(Legend,levels=df$var$Legend),
           scen_cpol=factor(scen_cpol,levels=lst$scen_cpol),scen_techpol=factor(scen_techpol,levels=lst$scen_techpol)) %>% 
    mutate(Value=Value/3.6/8760*10**6) %>% 
    filter(Value>0) %>% 
    mutate(Year=as.character(Year)) %>% 
    ggplot()+
    geom_point(aes(x=Year,y=Value,color=scen_cpol,shape=scen_techpol),show.legend=T)+
    scale_y_continuous(limits=c(0,NA),labels=scales::percent_format(accuracy=1))+
    facet_grid(.~Legend)+
    labs(x=NULL,y='Capacity factor (%)')+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_color_manual(values=lst$color_cpol)+
    scale_shape_manual(values=lst$shape_tech)+
    guides(color=guide_legend(title=NULL),shape=guide_legend(title=NULL))

p$tmp1 <- plot_grid(p$cap_ele_fos,p$cap_ele_hyd_cof,
                    nrow=1,rel_widths=c(1,1),labels=c('a','b'))
p$tmp <- plot_grid(p$tmp1,p$cap_fac,
                   ncol=1,rel_heights=c(1,1),labels=c('','c'))
ggsave(filename='output/fig2.png',plot=p$tmp,width=180,height=140,units='mm',dpi=300)


# Fig.3 -------------------------------------------------------------------

df$var <- tribble(~Variable,~Power,
                  'Sec_Ene_Ele_Sto_Dis_PHS_Sea','PHES+CAES',
                  'Sec_Ene_Ele_Sto_Dis_CAE','PHES+CAES',
                  'Sec_Ene_Ele_Coa_Hyd_Cof','Hydrogen co-fire',
                  'Sec_Ene_Ele_Gas_Hyd_Cof','Hydrogen co-fire')
p$sec_ene_ele_sea_sto <- df$all %>% filter(Region=='World',Year%in%c(2030,2050)) %>% 
    inner_join(df$var,by='Variable') %>% 
    group_by(Model,Region,Scenario,Power,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(Year=as.character(Year)) %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    ggplot()+
    geom_line(aes(x=scen_cpol,y=Value,color=scen_techpol,linetype=Power,group=interaction(Power,scen_techpol)),show.legend=T)+
    geom_point(aes(x=scen_cpol,y=Value,color=scen_techpol,shape=Power),show.legend=T)+
    facet_grid(.~Year)+
    labs(x=NULL,y=expression(paste('Electricity supply (EJ ',{yr}^{-1},')')))+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),
                       legend.box='vertical',legend.margin=margin(0,0,0,0))+
    scale_color_manual(values=lst$color_tech)+
    guides(color=guide_legend(title=NULL,override.aes=list(linetype=NULL)),shape=guide_legend(title=NULL),linetype=guide_legend(title=NULL))

df$var <- tribble(~Variable,~Label,
                  'Sec_Ene_Ele_Share_VRE','var_x',
                  'Sec_Ene_Ele_Cur','var_y',
                  'Sec_Ene_Ele_Fos_Hyd_Cof','var_z')
p$ele_cur <- df$all %>% filter(Region=='World') %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    select(-Unit,-Variable) %>% 
    pivot_wider(names_from='Label',values_from='Value') %>% 
    filter(var_x>0) %>% 
    ggplot()+
    geom_point(aes(x=var_x,y=var_y,color=scen_techpol,fill=scen_techpol,size=var_z),alpha=.5,shape=21,show.legend=T)+
    ylim(0,NA)+
    scale_x_continuous(limits=c(0,NA),labels=scales::percent_format(accuracy=1))+
    labs(x='VRE share (%)',y=expression(paste('Curtailment (EJ ',{yr}^{-1},')')))+
    mytheme$set1+theme(legend.position=c(.25,.65),strip.background=element_blank())+
    scale_color_manual(values=lst$color_tech)+
    scale_fill_manual(values=lst$color_tech)+
    guides(color='none',fill='none',size=guide_legend(title='Hydrogen co-firing\n(EJ per year)',override.aes=list(fill='grey50',alpha=.3)))

p$l_tmp <- get_legend(p$sec_ene_ele_sea_sto+guides(shape='none',linetype='none'))
p$tmp1 <- plot_grid(p$sec_ene_ele_sea_sto+guides(color='none')+theme(legend.position=c(.25,.85)),
                    p$ele_cur,nrow=1,rel_widths=c(1,1),labels=c('a','b'),axis='b',align='h')
p$tmp <- plot_grid(p$tmp1,p$l_tmp,ncol=1,rel_heights=c(1,.1))
ggsave(filename='output/fig3.png',plot=p$tmp,width=180,height=90,units='mm',dpi=300)


# Fig.4 -------------------------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Cap_Use_Ele_Coa','Coal-Used','grey30',
                  'Str_Cap_Ele_Coa','Coal-Unused','grey70',
                  'Cap_Use_Ele_Gas','Gas-Used','lightgoldenrod3',
                  'Str_Cap_Ele_Gas','Gas-Unused','lightgoldenrod1')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$cap_str_use <- df$all %>% filter(Region=='World') %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    filter(scen_cpol=='500',scen_techpol%in%c('NoCOF','H2Opt'),Year>=2020) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),
           scen_cpol=factor(scen_cpol,levels=lst$scen_cpol),scen_techpol=factor(scen_techpol,levels=c('NoCOF','H2Opt'))) %>% 
    ggplot()+
    geom_bar(aes(x=Year,y=Value,fill=Variable),stat='identity',position='stack',show.legend=T)+
    facet_grid(.~scen_techpol)+
    labs(x=NULL,y='Capacity (GW)')+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    guides(fill=guide_legend(title=NULL,nrow=2))

df$var <- tribble(~Variable,~Label,
                  'Str_Cap_Ele_Gas','Gas',
                  'Str_Cap_Ele_Coa','Coal')
p$str_cap <- df$all %>% filter(Region=='World',Year%in%c(2030,2050)) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(Label=factor(Label,levels=df$var$Label),
           scen_cpol=factor(scen_cpol,levels=lst$scen_cpol),scen_techpol=factor(scen_techpol,levels=lst$scen_techpol)) %>% 
    ggplot()+
    geom_line(aes(x=scen_cpol,y=Value,color=scen_techpol,group=scen_techpol),show.legend=T)+
    geom_point(aes(x=scen_cpol,y=Value,color=scen_techpol,fill=scen_techpol,shape=scen_techpol),show.legend=T)+
    ylim(0,NA)+
    facet_grid(~Label+Year)+
    labs(x=NULL,y='Unused capacity (GW)')+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_color_manual(values=lst$color_tech)+
    scale_fill_manual(values=lst$color_tech)+
    scale_shape_manual(values=lst$shape_tech)+
    guides(color=guide_legend(title=NULL,nrow=2),fill=guide_legend(title=NULL,nrow=2),shape=guide_legend(title=NULL,nrow=2))

p$tmp <- plot_grid(p$cap_str_use,p$str_cap,nrow=1,rel_widths=c(.7,1),labels=c('a','b'))
ggsave(filename='output/fig4.png',plot=p$tmp,width=180,height=90,units='mm',dpi=300)


# Fig.5 -------------------------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Line,
                  'LCO_Ele_Coa_USC_w_Car_Prc','Coal','without co-firing',
                  'LCO_Ele_Gas_ACC_w_Car_Prc','Gas','without co-firing',
                  'LCO_Ele_Coa_Hyd_Cof_USC_w_Car_Prc','Coal','50% co-firing',
                  'LCO_Ele_Gas_Hyd_Cof_ACC_w_Car_Prc','Gas','50% co-firing',
                  'LCO_Ele_Coa_Hyd_Cof_Ful_USC_w_Car_Prc','Coal','100% co-firing',
                  'LCO_Ele_Gas_Hyd_Cof_Ful_ACC_w_Car_Prc','Gas','100% co-firing',
                  'LCO_Ele_SolarPV_Mar','Solar PV','without co-firing')
lst$col <- c('Coal'='grey40','Gas'='lightgoldenrod3','Solar PV'='lightsalmon','Wind onshore'='lightskyblue3')
lst$lin <- c('without co-firing'='solid','50% co-firing'='dashed','100% co-firing'='dotted')
p$lcoe <- df$all %>% filter(Year>=2020,Region=='R5OECD90+EU') %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    filter(Year==2050) %>% 
    filter(scen_techpol!='NoCOF') %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    ggplot()+
    geom_line(aes(x=scen_cpol,y=Value,color=Legend,linetype=Line,group=Variable),show.legend=T)+
    geom_point(aes(x=scen_cpol,y=Value,color=Legend),show.legend=T)+
    ylim(0,NA)+
    facet_grid(.~scen_techpol)+
    labs(x=NULL,y=expression(paste('LCOE (US$ ',{MWh}^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank())+
    scale_color_manual(values=lst$col,labels=lst$leg,name=NULL)+
    scale_linetype_manual(values=lst$lin,name=NULL)

df$var <- tribble(~Variable,~Legend,~Cost,
                  'LCO_Cap_Cos_Ele_Coa_USC','0%','Capital',
                  'LCO_Cap_Cos_Ele_Coa_Hyd_Cof_USC','50%','Capital',
                  'LCO_Cap_Cos_Ele_Coa_Hyd_Cof_Ful_USC','100%','Capital',
                  'LCO_OM_Cos_Ele_Coa_USC','0%','O&M',
                  'LCO_OM_Cos_Ele_Coa_Hyd_Cof_USC','50%','O&M',
                  'LCO_OM_Cos_Ele_Coa_Hyd_Cof_Ful_USC','100%','O&M',
                  'LCO_Fue_Cos_Ele_Coa_USC','0%','Fuel',
                  'LCO_Fue_Cos_Ele_Coa_Hyd_Cof_USC','50%','Fuel',
                  'LCO_Fue_Cos_Ele_Coa_Hyd_Cof_Ful_USC','100%','Fuel',
                  'LCO_Emi_Cos_Ele_Coa_USC','0%','Emission',
                  'LCO_Emi_Cos_Ele_Coa_Hyd_Cof_USC','50%','Emission',
                  'LCO_Emi_Cos_Ele_Coa_Hyd_Cof_Ful_USC','100%','Emission')
lst$col <- c('Capital'='lightskyblue3','O&M'='moccasin','Fuel'='sandybrown','Emission'='darkolivegreen2')
lst$cof <- c('0%','50%','100%')
p$lcoe_col <- df$all %>% filter(Year>=2020,Region=='R5OECD90+EU') %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    filter(Year==2050) %>% 
    filter(scen_cpol%in%c('500','1000')) %>%
    filter(scen_techpol%in%c('Default','H2Opt','LimCCS')) %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol),
           Cost=factor(Cost,levels=rev(names(lst$col))),Legend=factor(Legend,levels=lst$cof)) %>% 
    ggplot()+
    geom_bar(aes(x=Legend,y=Value,fill=Cost),stat='identity',position='stack',show.legend=T)+
    ylim(0,NA)+
    facet_grid(.~scen_cpol+scen_techpol)+
    labs(title='Coal',x='Hydrogen mix rate',y=expression(paste('LCOE (US$ ',{MWh}^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),strip.text=element_text(size=8.5),
                       axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=lst$col,name=NULL)

df$var <- tribble(~Variable,~Legend,~Cost,
                  'LCO_Cap_Cos_Ele_Gas_ACC','0%','Capital',
                  'LCO_Cap_Cos_Ele_Gas_Hyd_Cof_ACC','50%','Capital',
                  'LCO_Cap_Cos_Ele_Gas_Hyd_Cof_Ful_ACC','100%','Capital',
                  'LCO_OM_Cos_Ele_Gas_ACC','0%','O&M',
                  'LCO_OM_Cos_Ele_Gas_Hyd_Cof_ACC','50%','O&M',
                  'LCO_OM_Cos_Ele_Gas_Hyd_Cof_Ful_ACC','100%','O&M',
                  'LCO_Fue_Cos_Ele_Gas_ACC','0%','Fuel',
                  'LCO_Fue_Cos_Ele_Gas_Hyd_Cof_ACC','50%','Fuel',
                  'LCO_Fue_Cos_Ele_Gas_Hyd_Cof_Ful_ACC','100%','Fuel',
                  'LCO_Emi_Cos_Ele_Gas_ACC','0%','Emission',
                  'LCO_Emi_Cos_Ele_Gas_Hyd_Cof_ACC','50%','Emission',
                  'LCO_Emi_Cos_Ele_Gas_Hyd_Cof_Ful_ACC','100%','Emission')
lst$col <- c('Capital'='lightskyblue3','O&M'='moccasin','Fuel'='sandybrown','Emission'='darkolivegreen2')
lst$cof <- c('0%','50%','100%')
p$lcoe_gas <- df$all %>% filter(Year>=2020,Region=='R5OECD90+EU') %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    filter(Year==2050) %>% 
    filter(scen_cpol%in%c('500','1000')) %>%
    filter(scen_techpol%in%c('Default','H2Opt','LimCCS')) %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scen_lab),
           scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol),
           Cost=factor(Cost,levels=rev(names(lst$col))),Legend=factor(Legend,levels=lst$cof)) %>% 
    ggplot()+
    geom_bar(aes(x=Legend,y=Value,fill=Cost),stat='identity',position='stack',show.legend=T)+
    ylim(0,NA)+
    facet_grid(.~scen_cpol+scen_techpol)+
    labs(title='Gas',x='Hydrogen mix rate',y=expression(paste('LCOE (US$ ',{MWh}^{-1},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),strip.text=element_text(size=8.5),
                       axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=lst$col,name=NULL)

p$l_tmp <- get_legend(p$lcoe_gas+theme(legend.position='bottom')+guides(fill=guide_legend(reverse=T)))
p$tmp1 <- plot_grid(p$lcoe_col+theme(legend.position='none'),p$lcoe_gas+theme(legend.position='none'),
                    nrow=1,rel_widths=c(1,1))
p$tmp <- plot_grid(p$lcoe,p$tmp1,p$l_tmp,ncol=1,rel_heights=c(1,1.1,.1),labels=c('a','b',''))
ggsave(filename='output/fig5.png',plot=p$tmp,width=180,height=150,units='mm',dpi=300)

