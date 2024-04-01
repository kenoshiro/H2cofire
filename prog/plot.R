mytheme <- list()
mytheme$set1 <- theme_bw()+theme(plot.title=element_text(size=8.5),
                                 panel.grid=element_blank(),
                                 panel.border=element_blank(),
                                 axis.line=element_line(color='black',linewidth=.3),
                                 axis.ticks=element_line(color='black',linewidth=.3),
                                 axis.text=element_text(size=8.5),
                                 axis.title=element_text(size=9),
                                 legend.title=element_text(size=9),
                                 legend.text=element_text(size=8))

df$scen_lab <- tribble(~Scenario,~scen_lab,~scen_wrap,
                       '500_Default','500_Default','500_Default',
                       '500_LimCCS','500_LimCCS','500_LimCCS',
                       '500_H2Opt','500_H2Opt','500_H2Opt',
                       '500_H2Opt+','500_H2Opt+','500_H2Opt+',
                       '500_NoCOF','500_NoCOF','500_NoCOF',
                       '700_Default','700_Default','700_Default',
                       '700_LimCCS','700_LimCCS','700_LimCCS',
                       '700_H2Opt','700_H2Opt','700_H2Opt',
                       '700_H2Opt+','700_H2Opt+','700_H2Opt+',
                       '700_NoCOF','700_NoCOF','700_NoCOF',
                       '1000_Default','1000_Default','1000_Default',
                       '1000_LimCCS','1000_LimCCS','1000_LimCCS',
                       '1000_H2Opt','1000_H2Opt','1000_H2Opt',
                       '1000_H2Opt+','1000_H2Opt+','1000_H2Opt+',
                       '1000_NoCOF','1000_NoCOF','1000_NoCOF',
                       '1400_Default','1400_Default','1400_Default',
                       '1400_LimCCS','1400_LimCCS','1400_LimCCS',
                       '1400_H2Opt','1400_H2Opt','1400_H2Opt',
                       '1400_H2Opt+','1400_H2Opt+','1400_H2Opt+',
                       '1400_NoCOF','1400_NoCOF','1400_NoCOF',
                       '500_H2Opt_SSP1','500_H2Opt_SSP1','500_H2Opt_SSP1',
                       '500_H2Opt_SSP3','500_H2Opt_SSP3','500_H2Opt_SSP3')
df$scen_cat <- tribble(~Scenario,~scen_cpol,~scen_techpol,
                       '500_Default','500','Default',
                       '500_LimCCS','500','LimCCS',
                       '500_H2Opt','500','H2Opt',
                       '500_H2Opt+','500','H2Opt+',
                       '500_NoCOF','500','NoCOF',
                       '700_Default','700','Default',
                       '700_LimCCS','700','LimCCS',
                       '700_H2Opt','700','H2Opt',
                       '700_H2Opt+','700','H2Opt+',
                       '700_NoCOF','700','NoCOF',
                       '1000_Default','1000','Default',
                       '1000_LimCCS','1000','LimCCS',
                       '1000_H2Opt','1000','H2Opt',
                       '1000_H2Opt+','1000','H2Opt+',
                       '1000_NoCOF','1000','NoCOF',
                       '1400_Default','1400','Default',
                       '1400_LimCCS','1400','LimCCS',
                       '1400_H2Opt','1400','H2Opt',
                       '1400_H2Opt+','1400','H2Opt+',
                       '1400_NoCOF','1400','NoCOF',
                       '500_H2Opt_SSP1','500','H2Opt_SSP1',
                       '500_H2Opt_SSP3','500','H2Opt_SSP3')
lst$scen_main <- df$scen_cat %>% filter(scen_techpol%in%c('Default','H2Opt','LimCCS','H2Opt+','NoCOF')) %>% .$Scenario
lst$scen_techpol <- c('Default','H2Opt','H2Opt+','LimCCS','NoCOF','H2Opt_SSP1','H2Opt_SSP3')
lst$scen_cpol <- c('500','700','1000','1400')
lst$R5 <- c('R5OECD90+EU','R5REF','R5ASIA','R5MAF','R5LAM')
df$R5 <- tribble(~Region,~R5,
                 'World','World',
                 'R5OECD90+EU','OECD\n& EU',
                 'R5REF','Reforming\nEconomies',
                 'R5ASIA','Asia',
                 'R5MAF','Middle East\n& Africa',
                 'R5LAM','Latin\nAmerica')

lst$color_cpol <- c('500'='salmon','700'='lightskyblue3','1000'='darkolivegreen2','1400'='lightgoldenrod3')
lst$color_tech <- c('Default'='#E64B35FF','H2Opt'='#3C5488FF','H2Opt+'='#4DBBD5FF','LimCCS'='#00A087FF','NoCOF'='#F39B7FFF','H2Opt_SSP1'='#7E6148FF','H2Opt_SSP3'='#91D1C2FF')
lst$shape_tech <- c('Default'=21,'H2Opt'=22,'LimCCS'=23,'H2Opt+'=24,'NoCOF'=25,'H2Opt_SSP1'=3,'H2Opt_SSP3'=4)

fcalc_range_category <- function(df_in){
    df_in %>% group_by(Variable,Category,Region,Year) %>% 
        summarise(p50=median(Value),p90=quantile(Value,.9),p10=quantile(Value,.1),p75=quantile(Value,.75),p25=quantile(Value,.25),p0=min(Value),p100=max(Value),n=n(),.groups='drop') %>% 
        return()
}

p$l_rangeleg <- tribble(~y,~label,
                        -.1,'Min',
                        1.1,'Max',
                        .5,'Median',
                        .2,'10th percentile',
                        .8,'90th percentile') %>% 
    mutate(x=0) %>% 
    ggplot()+
    geom_crossbar(aes(x=x),ymin=.2,ymax=.8,y=.5,width=.25,color='white',fill='grey')+
    geom_crossbar(aes(x=x),ymin=0,ymax=1,y=0,width=.25,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(y=y,label=label),x=.22,hjust=0,size=3)+
    labs(title='  AR6 range')+
    xlim(-.2,1.6)+ylim(-.2,1.3)+
    theme_void()+theme(plot.margin=unit(c(1,0,0,3),'mm'),plot.title=element_text(size=9))

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
    labs(x=NULL,y=expression(paste('Power generation (EJ ',{yr}^{`-1`},')')))+
    mytheme$set1+
    theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),legend.key.size=unit(5.5,'mm'))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    guides(fill=guide_legend(title=NULL))

p$tmp1 <- df$all %>% filter(Region=='World',Year>=2020) %>% 
    filter(Variable=='Sec_Ene_Ele_Fos',Scenario%in%lst$scen_main) %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=scen_cpol,group=interaction(scen_cpol,scen_techpol)),linewidth=.3,show.legend=T)+
    geom_point(aes(x=Year,y=Value,color=scen_cpol,shape=scen_techpol),fill='white',show.legend=T)+
    scale_y_continuous(limits=c(0,NA))+
    coord_cartesian(ylim=c(0,155))+
    labs(x=NULL,y=expression(paste('Power generation (EJ ',{yr}^{`-1`},')')))+
    mytheme$set1+
    theme(legend.position='right',strip.background=element_blank(),legend.margin=margin(0,0,0,0),
          axis.text.x=element_text(angle=45,hjust=1))+
    scale_color_manual(values=lst$color_cpol)+
    scale_shape_manual(values=lst$shape_tech)+
    guides(color=guide_legend(title=NULL,override.aes=list(linetype=NA)),
           shape=guide_legend(title=NULL,override.aes=list(linetype=NA)))
p$l_tmp1 <- get_legend(p$tmp1+theme(legend.box='horizontal'))
p$tmp2 <- df$load_AR6_global %>% 
    filter(Variable=='Sec_Ene_Ele_Fos',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p100),size=2,angle=90,hjust=1.2,vjust=.5)+
    scale_y_continuous(limits=c(0,NA))+
    coord_cartesian(ylim=c(0,155))+
    labs(x=NULL,y=NULL)+
    mytheme$set1+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5),
          axis.text.y.left=element_blank(),axis.line.y.left=element_blank(),axis.ticks.y.left=element_blank())
p$sec_ene_ele_fos <- plot_grid(p$tmp1+theme(legend.position='none',plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt')),
                                p$tmp2+theme(plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt')),
                                rel_widths=c(1,.25),axis='tb',align='h')

p$share_cof <- df$all %>% filter(Scenario%in%lst$scen_main,Region=='World',Year%in%c(2030,2050)) %>% 
    filter(Variable=='Sec_Ene_Ele_Share_Hyd_Cof_all') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    ggplot()+
    geom_line(aes(x=scen_cpol,y=Value,color=scen_techpol,group=scen_techpol))+
    geom_point(aes(x=scen_cpol,y=Value,color=scen_techpol,fill=scen_techpol,shape=scen_techpol),show.legend=T)+
    facet_grid(.~Year)+
    scale_y_continuous(limits=c(0,NA),labels=scales::percent_format(accuracy=.1))+
    labs(title='World',x=expression(paste('Cumulative emissions (','Gt-',{CO[2]},')')),y='Share of hydrogen co-firing\nin total generation')+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_color_manual(values=lst$color_tech)+
    scale_fill_manual(values=lst$color_tech)+
    scale_shape_manual(values=lst$shape_tech)+
    guides(color=guide_legend(title=NULL),fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))
p$share_cof_R5 <- df$all %>% filter(Scenario%in%lst$scen_main,Region%in%c(lst$R5),Year%in%c(2030,2050)) %>% 
    filter(Variable=='Sec_Ene_Ele_Share_Hyd_Cof_all') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    inner_join(df$R5,by='Region') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scen_main),
           scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    ggplot()+
    geom_line(aes(x=scen_cpol,y=Value,color=scen_techpol,group=scen_techpol))+
    geom_point(aes(x=scen_cpol,y=Value,color=scen_techpol,fill=scen_techpol,shape=scen_techpol),show.legend=T)+
    facet_grid(Year~R5)+
    scale_y_continuous(limits=c(0,NA),labels=scales::percent_format(accuracy=.1))+
    labs(x=expression(paste('Cumulative emissions (','Gt-',{CO[2]},')')),y='Share of hydrogen co-firing\nin total generation')+
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
    labs(x=expression(paste('Carbon prices (US$ ','t-',{CO[2]}^{`-1`},')')),y='Share of hydrogen co-firing\nin thermal generation')+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),
                       legend.margin=margin(0,0,0,0))+
    scale_color_manual(values=lst$color_cpol)+
    scale_shape_manual(values=lst$shape_tech)+
    guides(color=guide_legend(title=NULL),shape=guide_legend(title=NULL))

p$l_tmp <- plot_grid(p$l_tmp1,ggplotGrob(p$l_rangeleg+theme(plot.margin=margin(50,0,50,0))),
                     get_legend(p$share_cof),nrow=1,rel_widths=c(1.3,1,1))
p$tmp1 <- plot_grid(p$sec_ene_ele,p$sec_ene_ele_fos+theme(legend.position='none'),
                    nrow=1,rel_widths=c(1,.67),labels=c('a','b'),label_size=12)
p$tmp2 <- plot_grid(p$share_cof+theme(legend.position='none'),p$share_cof_R5+theme(legend.position='none'),
                    nrow=1,rel_widths=c(1,2),labels=c('c','d'),label_size=12)
p$tmp3 <- plot_grid(p$cof_carpri+theme(legend.position='none'),p$l_tmp,
                    nrow=1,rel_widths=c(1,1.4),labels=c('e',''),label_size=12)
p$tmp <- plot_grid(p$tmp1,p$tmp2,p$tmp3,ncol=1,rel_heights=c(1,1.25,1))
ggsave(filename='output/fig1.png',plot=p$tmp,width=180,height=200,units='mm',dpi=300,bg='white')
ggsave(filename='output/fig1.eps',plot=p$tmp,width=180,height=200,units='mm')

# Fig.2 -------------------------------------------------------------------

df$var <- tribble(~Variable,~Color,~Energy,~Status,
                  'Cap_Ele_Coa_Una','grey70','Coal','unabated',
                  'Cap_Ele_Coa_Hyd_Cof','grey50','Coal','H2 co-firing',
                  'Cap_Ele_Coa_w_CCS','grey20','Coal','CCS',
                  'Cap_Ele_Gas_Una','lightgoldenrod1','Gas','unabated',
                  'Cap_Ele_Gas_Hyd_Cof','lightgoldenrod3','Gas','H2 co-firing',
                  'Cap_Ele_Gas_w_CCS','lightgoldenrod4','Gas','CCS')
lst$leg <- as.character(df$var$Status); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$tmp <- df$all %>% filter(Region=='World',Year%in%seq(2020,2050,5)) %>% 
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
    scale_fill_manual(values=rev(lst$col),name=NULL)+
    guides(fill=guide_legend(title=NULL))
p$l_tmp1 <- df$var %>% 
    mutate(Value=1) %>% filter(Energy=='Gas') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable))) %>% 
    ggplot()+geom_bar(aes(x=Energy,y=Value,fill=Variable),stat='identity',show.legend=T)+
    theme(legend.title=element_text(size=8),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=rev(lst$col),labels=rev(lst$leg),name=NULL)+
    guides(fill=guide_legend(title='Gas'))
p$l_tmp2 <- df$var %>% 
    mutate(Value=1) %>% filter(Energy=='Coal') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable))) %>% 
    ggplot()+geom_bar(aes(x=Energy,y=Value,fill=Variable),stat='identity',show.legend=T)+
    theme(legend.title=element_text(size=8),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=rev(lst$col),labels=rev(lst$leg),name=NULL)+
    guides(fill=guide_legend(title='Coal'))
p$l_tmp <- plot_grid(NULL,get_legend(p$l_tmp1),get_legend(p$l_tmp2),NULL,ncol=1,rel_heights=c(1,2.5,2.5,1))
p$cap_ele_fos <- plot_grid(p$tmp+theme(legend.position='none'),p$l_tmp,nrow=1,rel_widths=c(1,.45))

df$var <- tribble(~Variable,~Shape,
                  'Cap_Ele_Coa_and_Gas_Hyd_Cof','with H2\nco-firing',
                  'Cap_Ele_Fos_and_Hyd','Total')
lst$shp <- c('Total'=21,'with H2\nco-firing'=16)
p$tmp1 <- df$all %>% filter(Scenario%in%lst$scen_main,Region=='World',Year%in%c(2030,2050)) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol),
           Shape=factor(Shape,levels=names(lst$shp))) %>% 
    ggplot()+
    geom_point(aes(x=scen_cpol,y=Value,color=scen_techpol,shape=Shape),show.legend=T)+
    facet_grid(.~Year)+
    scale_y_continuous(limits=c(0,NA))+
    coord_cartesian(ylim=c(0,8200))+
    labs(x=expression(paste('Cumulative emissions (','Gt-',{CO[2]},')')),y='Capacity (GW)')+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))+
    scale_shape_manual(values=lst$shp,name=NULL)+
    scale_color_manual(values=lst$color_tech,name=NULL)
p$tmp2 <- df$load_AR6_global %>% 
    filter(Variable=='Cap_Ele_Fos',Year=='2050',Category%in%c('C1','C2','C3')) %>% 
    fcalc_range_category() %>% 
    ggplot()+
    geom_crossbar(aes(x=Category,ymin=p10,ymax=p90,y=p50),width=.75,color='white',fill='grey')+
    geom_crossbar(aes(x=Category,ymin=p0,ymax=p100,y=p0),width=.75,color='grey',fill='transparent',fatten=0)+
    geom_text(aes(x=Category,label=str_c('n=',n),y=p100),size=2,angle=90,hjust=1.2,vjust=.5)+
    scale_y_continuous(limits=c(0,NA))+
    coord_cartesian(ylim=c(0,8200))+
    # scale_y_continuous(limits=c(0,1),sec.axis=sec_axis(~.,labels=scales::percent_format(accuracy=1),name='Final energy share (%)'))+
    labs(x=NULL,y=NULL)+
    mytheme$set1+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5),
          axis.text.y.left=element_blank(),axis.line.y.left=element_blank(),axis.ticks.y.left=element_blank())
p$l_tmp1 <- get_legend(p$tmp1)
p$cap_ele_hyd_cof <- plot_grid(p$tmp1+theme(legend.position='none',plot.margin=unit(c(5.5,0,5.5,5.5),unit='pt')),
                               p$tmp2+theme(plot.margin=unit(c(5.5,5.5,5.5,0),unit='pt')),p$l_tmp1,
                               rel_widths=c(1,.2,.45),nrow=1,axis='tb',align='h')

df$var <- tribble(~Variable,~Legend,~Color,
                  'Cap_Fac_Ele_Gas_wo_CCS','Gas\nunabated','lightgoldenrod4',
                  'Cap_Fac_Ele_Gas_w_CCS','Gas\nwith CCS','lightgoldenrod3',
                  'Cap_Fac_Ele_Gas_Hyd_Cof','Gas\nwith H2 co-firing','lightgoldenrod',
                  'Cap_Fac_Ele_Coa_wo_CCS','Coal\nunabated','grey10',
                  'Cap_Fac_Ele_Coa_w_CCS','Coal\nwith CCS','grey30',
                  'Cap_Fac_Ele_Coa_Hyd_Cof','Coal\nwith H2 co-firing','grey50')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$cap_fac <- df$all %>% filter(Scenario%in%lst$scen_main,Region%in%c('World'),Year%in%c(2030,2050)) %>% 
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
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),strip.clip='off')+
    scale_color_manual(values=lst$color_cpol)+
    scale_shape_manual(values=lst$shape_tech)+
    guides(color=guide_legend(title=NULL),shape=guide_legend(title=NULL))

p$tmp1 <- plot_grid(p$cap_ele_fos,p$cap_ele_hyd_cof,nrow=1,rel_widths=c(1,1.2),labels=c('a','b'),label_size=12)
p$tmp <- plot_grid(p$tmp1,p$cap_fac,ncol=1,rel_heights=c(1,1),labels=c('','c'),label_size=12)
ggsave(filename='output/fig2.png',plot=p$tmp,width=180,height=140,units='mm',dpi=300)
ggsave(filename='output/fig2.eps',plot=p$tmp,width=180,height=140,units='mm')


# Fig.3 -------------------------------------------------------------------

df$var <- tribble(~Variable,~Power,
                  'Sec_Ene_Ele_Sto_Dis_PHS_Sea','PHES+CAES',
                  'Sec_Ene_Ele_Sto_Dis_CAE','PHES+CAES',
                  'Sec_Ene_Ele_Coa_Hyd_Cof','Hydrogen co-fire',
                  'Sec_Ene_Ele_Gas_Hyd_Cof','Hydrogen co-fire')
p$sec_ene_ele_sea_sto <- df$all %>% filter(Scenario%in%lst$scen_main,Region=='World',Year%in%c(2030,2050)) %>% 
    inner_join(df$var,by='Variable') %>% 
    group_by(Model,Region,Scenario,Power,Year) %>% summarise(Value=sum(Value),.groups='drop') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(Year=as.character(Year)) %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    ggplot()+
    geom_line(aes(x=scen_cpol,y=Value,color=scen_techpol,linetype=Power,group=interaction(Power,scen_techpol)),show.legend=T)+
    geom_point(aes(x=scen_cpol,y=Value,color=scen_techpol,shape=Power),show.legend=T)+
    facet_grid(.~Year)+
    labs(x=expression(paste('Cumulative emissions (','Gt-',{CO[2]},')')),y=expression(paste('Electricity supply (EJ ',{yr}^{`-1`},')')))+
    mytheme$set1+theme(legend.position='bottom',strip.background=element_blank(),
                       legend.box='vertical',legend.margin=margin(0,0,0,0))+
    scale_color_manual(values=lst$color_tech)+
    guides(color=guide_legend(title=NULL,override.aes=list(linetype=NULL)),shape=guide_legend(title=NULL),linetype=guide_legend(title=NULL))

df$var <- tribble(~Variable,~Label,
                  'Sec_Ene_Ele_Share_VRE','var_x',
                  'Sec_Ene_Ele_Cur','var_y',
                  'Sec_Ene_Ele_Fos_Hyd_Cof','var_z')
p$ele_cur <- df$all %>% filter(Scenario%in%lst$scen_main,Region=='World') %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scen_main),
           scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol)) %>% 
    select(-Unit,-Variable) %>% 
    pivot_wider(names_from='Label',values_from='Value') %>% 
    filter(var_x>0) %>% 
    ggplot()+
    geom_point(aes(x=var_x,y=var_y,color=scen_techpol,fill=scen_techpol,size=var_z),alpha=.5,shape=21,show.legend=T)+
    ylim(0,NA)+
    scale_x_continuous(limits=c(0,NA),labels=scales::percent_format(accuracy=1))+
    labs(x='VRE share (%)',y=expression(paste('Curtailment (EJ ',{yr}^{`-1`},')')))+
    mytheme$set1+theme(legend.position=c(.25,.65),strip.background=element_blank())+
    scale_color_manual(values=lst$color_tech)+
    scale_fill_manual(values=lst$color_tech)+
    guides(color='none',fill='none',size=guide_legend(title='Power generation with\nhydrogen co-firing\n(EJ per year)',override.aes=list(fill='grey50',alpha=.3)))

p$l_tmp <- get_legend(p$sec_ene_ele_sea_sto+guides(shape='none',linetype='none'))
p$tmp1 <- plot_grid(p$sec_ene_ele_sea_sto+guides(color='none')+theme(legend.position=c(.25,.85)),
                    p$ele_cur,nrow=1,rel_widths=c(1,1),labels=c('a','b'),label_size=12,axis='b',align='h')
p$tmp <- plot_grid(p$tmp1,p$l_tmp,ncol=1,rel_heights=c(1,.1))
ggsave(filename='output/fig3.png',plot=p$tmp,width=180,height=90,units='mm',dpi=300)
ggsave(filename='output/fig3.pdf',plot=p$tmp,width=180,height=90,units='mm',device=cairo_pdf)


# Fig.4 -------------------------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,~Energy,~Status,
                  'Cap_Use_Ele_Coa','Coal-Used','grey30','Coal','Used',
                  'Str_Cap_Ele_Coa','Coal-Unused','grey70','Coal','Unused',
                  'Cap_Use_Ele_Gas','Gas-Used','lightgoldenrod3','Gas','Used',
                  'Str_Cap_Ele_Gas','Gas-Unused','lightgoldenrod1','Gas','Unused')
lst$leg <- as.character(df$var$Legend); names(lst$leg) <- as.character(df$var$Variable)
lst$leg2 <- as.character(df$var$Status); names(lst$leg) <- as.character(df$var$Variable)
lst$col <- as.character(df$var$Color); names(lst$col) <- as.character(df$var$Variable)
p$tmp <- df$all %>% filter(Region=='World') %>% 
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
p$l_tmp1 <- df$var %>% 
    mutate(Value=1) %>% filter(Energy=='Gas') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable))) %>% 
    ggplot()+geom_bar(aes(x=Energy,y=Value,fill=Variable),stat='identity',show.legend=T)+
    theme(legend.title=element_text(size=8),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=rev(lst$col),labels=rev(lst$leg2),name=NULL)+
    guides(fill=guide_legend(title='Gas'))
p$l_tmp2 <- df$var %>% 
    mutate(Value=1) %>% filter(Energy=='Coal') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable))) %>% 
    ggplot()+geom_bar(aes(x=Energy,y=Value,fill=Variable),stat='identity',show.legend=T)+
    theme(legend.title=element_text(size=8),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=rev(lst$col),labels=rev(lst$leg2),name=NULL)+
    guides(fill=guide_legend(title='Coal'))
p$l_tmp <- plot_grid(NULL,get_legend(p$l_tmp1),get_legend(p$l_tmp2),NULL,ncol=1)
p$cap_str_use <- plot_grid(p$tmp+theme(legend.position='none'),p$l_tmp,nrow=1,rel_widths=c(1,.3))

df$var <- tribble(~Variable,~Label,
                  'Str_Cap_Ele_Gas','Gas',
                  'Str_Cap_Ele_Coa','Coal')
p$str_cap <- df$all %>% filter(Scenario%in%lst$scen_main,Region=='World',Year%in%c(2030,2050)) %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    mutate(Label=factor(Label,levels=df$var$Label),
           scen_cpol=factor(scen_cpol,levels=lst$scen_cpol),scen_techpol=factor(scen_techpol,levels=lst$scen_techpol)) %>% 
    ggplot()+
    geom_line(aes(x=scen_cpol,y=Value,color=scen_techpol,group=scen_techpol),show.legend=T)+
    geom_point(aes(x=scen_cpol,y=Value,color=scen_techpol,shape=scen_techpol),fill='transparent',show.legend=T)+
    ylim(0,NA)+
    facet_grid(~Label+Year)+
    labs(x=expression(paste('Cumulative emissions (','Gt-',{CO[2]},')')),y='Unused capacity (GW)')+
    mytheme$set1+
    theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),legend.margin=margin(0,0,0,0))+
    scale_color_manual(values=lst$color_tech)+
    scale_fill_manual(values=lst$color_tech)+
    scale_shape_manual(values=lst$shape_tech)+
    guides(color=guide_legend(title=NULL),fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))

df$reg <- tribble(~Region,~Legend,~Color,
                  'R5ASIA','Asia','#8DA0CB',
                  'R5MAF','Middle East\n& Africa','#E5C494',
                  'R5OECD90+EU','OECD & EU','#66C2A5',
                  'R5REF','Reforming\nEconomies','#E78AC3',
                  'R5LAM','Latin\nAmerica','#FFD92F')
lst$leg <- as.character(df$reg$Legend); names(lst$leg) <- as.character(df$reg$Region)
lst$col <- as.character(df$reg$Color); names(lst$col) <- as.character(df$reg$Region)
p$tmp1 <- df$all %>% filter(Region%in%lst$R5,Variable=='Str_Cap_Ele_Gas') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_main,Year%in%seq(2030,2050,10),scen_cpol=='500') %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),Region=factor(Region,levels=rev(df$reg$Region))) %>% 
    ggplot()+
    geom_bar(aes(x=Year,y=Value,fill=Region),stat='identity',position='stack',show.legend=T)+
    facet_grid(.~scen_techpol)+
    labs(title='Gas',x=NULL,y='Unused capacity (GW)')+
    ylim(0,1500)+
    mytheme$set1+
    theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),strip.clip='off',legend.spacing.y=unit(3,'mm'))+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    guides(fill=guide_legend(title=NULL))
p$tmp2 <- df$all %>% filter(Region%in%lst$R5,Variable=='Str_Cap_Ele_Coa') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    filter(Scenario%in%lst$scen_main,Year%in%seq(2030,2050,10),scen_cpol=='500') %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),Region=factor(Region,levels=rev(df$reg$Region))) %>% 
    ggplot()+
    geom_bar(aes(x=Year,y=Value,fill=Region),stat='identity',position='stack',show.legend=T)+
    facet_grid(.~scen_techpol)+
    labs(title='Coal',x=NULL,y='Unused capacity (GW)')+
    ylim(0,1500)+
    mytheme$set1+
    theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),strip.clip='off')+
    scale_fill_manual(values=rev(lst$col),labels=lst$leg,name=NULL)+
    guides(fill=guide_legend(title=NULL))
p$str_cap_R5 <- plot_grid(p$tmp1+theme(legend.position='none'),
                          p$tmp2+theme(axis.text.y.left=element_blank(),axis.line.y.left=element_blank(),axis.ticks.y.left=element_blank(),axis.title.y=element_blank()),
                          nrow=1,rel_widths=c(1,1.28))

p$tmp1 <- plot_grid(p$cap_str_use,p$str_cap,nrow=1,rel_widths=c(.75,1),labels=c('a','b'),label_size=12)
p$tmp <- plot_grid(p$tmp1,p$str_cap_R5,ncol=1,rel_heights=c(1,1),labels=c('','c'),label_size=12)
print(p$tmp)
ggsave(filename='output/fig4.png',plot=p$tmp,width=180,height=140,units='mm',dpi=300)
ggsave(filename='output/fig4.eps',plot=p$tmp,width=180,height=140,units='mm')


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
p$lcoe <- df$all %>% filter(Scenario%in%lst$scen_main,Year>=2020,Region=='R5OECD90+EU') %>% 
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
    labs(x=expression(paste('Cumulative emissions (','Gt-',{CO[2]},')')),y=expression(paste('LCOE (US$ ',{MWh}^{`-1`},')')))+
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
p$lcoe_col <- df$all %>% filter(Scenario%in%lst$scen_main,Year>=2020,Region=='R5OECD90+EU') %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    filter(Year==2050) %>% 
    filter(scen_cpol%in%c('500','1000')) %>%
    filter(scen_techpol%in%c('Default','H2Opt','H2Opt+','LimCCS')) %>% 
    mutate(scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol),
           Cost=factor(Cost,levels=rev(names(lst$col))),Legend=factor(Legend,levels=lst$cof)) %>% 
    ggplot()+
    geom_bar(aes(x=Legend,y=Value,fill=Cost),stat='identity',position='stack',show.legend=T)+
    ylim(0,NA)+
    facet_grid(.~scen_cpol+scen_techpol)+
    labs(title='Coal',x='Hydrogen mix rate',y=expression(paste('LCOE (US$ ',{MWh}^{`-1`},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),strip.text=element_text(size=8),
                       axis.text.x=element_text(angle=90,hjust=1,vjust=.5,size=8),strip.clip='off')+
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
p$lcoe_gas <- df$all %>% filter(Scenario%in%lst$scen_main,Year>=2020,Region=='R5OECD90+EU') %>% 
    inner_join(df$var,by='Variable') %>% 
    inner_join(df$scen_cat,by='Scenario') %>% 
    filter(Year==2050) %>% 
    filter(scen_cpol%in%c('500','1000')) %>%
    filter(scen_techpol%in%c('Default','H2Opt','H2Opt+','LimCCS')) %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scen_lab),
           scen_techpol=factor(scen_techpol,levels=lst$scen_techpol),scen_cpol=factor(scen_cpol,levels=lst$scen_cpol),
           Cost=factor(Cost,levels=rev(names(lst$col))),Legend=factor(Legend,levels=lst$cof)) %>% 
    ggplot()+
    geom_bar(aes(x=Legend,y=Value,fill=Cost),stat='identity',position='stack',show.legend=T)+
    ylim(0,NA)+
    facet_grid(.~scen_cpol+scen_techpol)+
    labs(title='Gas',x='Hydrogen mix rate',y=expression(paste('LCOE (US$ ',{MWh}^{`-1`},')')))+
    mytheme$set1+theme(legend.position='right',strip.background=element_blank(),strip.text=element_text(size=8),
                       axis.text.x=element_text(angle=90,hjust=1,vjust=.5,size=8),strip.clip='off')+
    scale_fill_manual(values=lst$col,name=NULL)

p$l_tmp <- get_legend(p$lcoe_gas+theme(legend.position='bottom')+guides(fill=guide_legend(reverse=T)))
p$tmp1 <- plot_grid(p$lcoe_gas+theme(legend.position='none'),p$lcoe_col+theme(legend.position='none',axis.title.y=element_blank()),
                    nrow=1,rel_widths=c(1.09,1))
p$tmp <- plot_grid(p$lcoe,p$tmp1,p$l_tmp,ncol=1,rel_heights=c(1,1.2,.1),labels=c('a','b',''),label_size=12)
ggsave(filename='output/fig5.png',plot=p$tmp,width=180,height=150,units='mm',dpi=300,bg='white')
ggsave(filename='output/fig5.eps',plot=p$tmp,width=180,height=150,units='mm')
