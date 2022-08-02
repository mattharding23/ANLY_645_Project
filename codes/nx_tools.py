import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import os
import imageio.v2 as imageio
import statistics
from sklearn.linear_model import LinearRegression
import statsmodels.formula.api as smf
import math
from scipy.optimize import curve_fit
from scipy.stats import linregress

#------------------------------
# ANIMATION PLOT
#------------------------------
def animate(networks,pos_type="circular",path="output.gif",plot_every=1):
    
    count = 1
    file_names = []
    cmap=plt.cm.get_cmap('Blues')
    

        
    for G in networks:
        fig, ax = plt.subplots()
        fig.set_size_inches(20, 20)
        direct = nx.is_directed(G)
        if direct:
            degs_in = [x[1] for x in G.in_degree]
            degs_out = [x[1] for x in G.out_degree]
            if pos_type == "circular": 
                pos = nx.circular_layout(G)
            elif pos_type == "spring":
                pos = nx.spring_layout(G)
            elif pos_type == "random":   
                pos = nx.random_layout(G)
            
            nx.draw(G,
                    node_color = [cmap(x/(0.1+max(degs_out))) for x in degs_out],
                    node_size = [4000*x/(0.01+max(degs_in)) for x in degs_in],
                    pos = pos)
                
        else:
            degs = [x[1] for x in G.degree]
            if pos_type == "circular": 
                pos = nx.circular_layout(G)
            elif pos_type == "spring":
                pos = nx.spring_layout(G)
            elif pos_type == "random":   
                nx.random_layout(G)
            
                       
            nx.draw(
                G,
                node_color= [cmap(x/(0.1+max(degs))) for x in degs],
                node_size= [4000*x/(0.01+max(degs)) for x in degs],
                pos = pos)
            
            
        tmp_path = "tmp"+str(count)+".png"
        file_names.append(tmp_path)
        plt.savefig(tmp_path)
        count += 1
        plt.close(fig)
    
    #INITIALIZE FIGURE AND PLOT
    fig, ax = plt.subplots()  
    fig.set_size_inches(5, 5)

    #GET MIN AND MAX POSITION
    tmpx=[]; tmpy=[]
    for i in pos.keys():
        tmpx.append(pos[i][0])
        tmpy.append(pos[i][1])
        Lxmin=min(tmpx)-0.2; Lxmax=max(tmpx)+0.2
        Lymin=min(tmpy)-0.2; Lymax=max(tmpy)+0.2

        #DRAW BOX
        ax.axhline(y=Lymin)
        ax.axvline(x=Lxmin)
        ax.axhline(y=Lymax)
        ax.axvline(x=Lxmax)
    
    import imageio.v2 as imageio
    images = list(map(lambda filename: imageio.imread(filename), file_names))
    imageio.mimsave(path, images, duration = 10/len(file_names) ) # modify the frame duration as needed
    
    from IPython.display import display,Image
    display(Image(data=open(path,'rb').read(), format='png', width=800))
    
    for f in file_names:
        os.remove(f)
        
        
#------------------------------
# NETWORK CENTRALITY CORRELATION PLOTS
#------------------------------
def plot_centrality_correlation(G,path=""):
    direct = nx.is_directed(G)
    if(direct):
        degs_in = [x[1] for x in G.in_degree]
        degs_out = [x[1] for x in G.out_degree]
        cc_in = nx.closeness_centrality(G).values()
        G_out = G.reverse()
        cc_out = nx.closeness_centrality(G_out).values()
        bc = nx.betweenness_centrality(G).values()
        df = pd.DataFrame({"In Degree":degs_in,
                           "Out Degree":degs_out,
                           "In Closeness":cc_in,
                           "Out Closeness":cc_out,
                           "Betweenness":bc})
        sns.pairplot(df)
        if(path != ""):
            plt.savefig(path)
    
        plt.show()
    else:
        degs = [x[1] for x in G.degree]
        cc = nx.closeness_centrality(G).values()
        bc = nx.betweenness_centrality(G).values()
        df = pd.DataFrame({"Degree":degs,"Closeness":cc,"Betweenness":bc})
        sns.pairplot(df)
        if(path != ""):
            plt.savefig(path)
    
        plt.show()
     
        
    

#------------------------------
# AVERAGE DEGREE
#------------------------------
def ave_degree(G):
    direct = nx.is_directed(G)
    if(direct):
        degs_in = [x[1] for x in G.in_degree]
        degs_out = [x[1] for x in G.out_degree]
        print("Average in Degree: ",statistics.mean(degs_in)
              ,"\nAverage out Degree: ",statistics.mean(degs_out))
    else:
        degs = [x[1] for x in G.degree]
        print("Average Degree: ",statistics.mean(degs))

#------------------------------
# PLOT DEGREE DISTRIBUTION
#------------------------------

def plot_degrees(degs,path,fit):
    new_degs = []
    for d in degs:
        if d[0] > 0:
            if d[1] > 0:
                new_degs.append(d)
    degs = new_degs
        
    #PARAM
    BINS=20
    FS=9

    #INITIALIZE MPL FIGURE+AX
    fig, ax = plt.subplots(1, 4) # or what ever layout you want
    fig.set_size_inches(25, 5)
    
    
    #------------------
    #LOG-LOG PDF
    #------------------
    #GET DATA POINTS 
    y=np.array(list(dict(degs).values())); # print(len(G.nodes()),y.shape)
    bin_counts, bin_edges, patches = ax[1].hist(y, bins=BINS); ax[1].clear()
    bin_points=(np.array(bin_edges[1:])+np.array(bin_edges[0:-1]))/2.0
    # bin_points=np.array(bin_edges[1:])
    dx=bin_points[1]-bin_points[0]
    y=np.array(bin_counts)/len(y) #/dx;   #print(y, len(bin_points),len(bin_counts))
    

    #PLOT
    ax[0].plot(bin_points,y,"o",color = "blue")
    # ax[0,1].set_xlim([1, 1.1*max(bin_points)])
    # ax[0,1].set_ylim([0.0000001, 1])
    ax[0].set_ylabel("Probability",fontsize=FS)
    ax[0].set_xlabel("Degree",fontsize=FS)

    ax[0].set_xscale("log")
    ax[0].set_yscale("log")
    ax[0].set_aspect('auto', 'box')
    
    if fit:
        newX = np.logspace(0, 2, base=10)  


        def myExpFunc(x, a, b):
            return a * np.power(x, b)
        popt, pcov = curve_fit(myExpFunc, bin_points, y)
        ax[0].plot(newX, myExpFunc(newX, *popt), '-', color = "orange")
        
        
        
        #log_bin_points = np.log(bin_points) 
        #y = y + 0.0001
        #log_y = np.log(y)

        #m, b = np.polyfit(log_bin_points, y, 1)
        #lin_reg = np.exp(m*log_bin_points + b)
        #lin_reg = [m*x+b for x in log_bin_points]

        #add linear regression line to scatterplot 
        #ax[0].plot(log_bin_points, lin_reg,"-", color = "blue")

    #------------------
    #PDF
    #------------------
    df=pd.DataFrame(degs); #print(df)
    sns.histplot(data=df, x=1,bins=BINS,stat="probability", kde=False,ax=ax[1])
    ax[1].set_xlabel("Degree",fontsize=FS)
    ax[1].set_ylabel("Probability",fontsize=FS)
    ax[1].set_aspect('auto', 'box')

    

    #------------------
    #cCDF
    #------------------
    sns.ecdfplot(data=df, complementary=True, x=1,ax=ax[2])
    ax[2].set_ylabel("cCDF",fontsize=FS)
    ax[2].set_xlabel("Degree",fontsize=FS)
    ax[2].set_aspect('auto', 'box')

    #------------------
    #cCDF log-log
    #------------------
 
    sns.ecdfplot(data=df, complementary=True, x=1,ax=ax[3], color = "blue")
    
    if fit:
        res = linregress(df[1],np.array(df[1])/len(df[1]))
        y = [res.slope*x + res.intercept for x in df[1]]
        df[1] = y
        sns.ecdfplot(data= df, complementary=True, x=1,ax=ax[3], color = "orange")
    ax[3].set_ylabel("cCDF",fontsize=FS)
    ax[3].set_xscale("log")
    ax[3].set_yscale("log")
    ax[3].set_xlabel("Degree",fontsize=FS)
    ax[3].set_aspect('auto', 'box')
    

    
    if path != "":
        plt.savefig(path)
    plt.show()




def plot_degree_distribution(G,type="in",path="",fit=False):
    
    direct = nx.is_directed(G)
    if direct:
        if type == "in":
            degs = G.in_degree()
            plot_degrees(degs,path,fit)
        elif type == "out":
            degs = G.out_degree()
            plot_degrees(degs,path,fit)
        else:
            return(print("Error: graph is directed & the direction of degree needs to be specified."))
    else:
        degs = G.degree()
        plot_degrees(degs,path,fit)
        
                     
    
    
#------------------------------
# NETWORK PLOTTING FUNCTION
#------------------------------
def plot_network(G,node_color="degree",layout="random"):
    
    # POSITIONS LAYOUT
    N=len(G.nodes)
    if(layout=="spring"):
        # pos=nx.spring_layout(G,k=50*1./np.sqrt(N),iterations=100)
        pos=nx.spring_layout(G)

    if(layout=="random"):
        pos=nx.random_layout(G)

    #INITALIZE PLOT
    fig, ax = plt.subplots()
    fig.set_size_inches(15, 15)

    # NODE COLORS
    cmap=plt.cm.get_cmap('Greens')

    # DEGREE 
    if node_color=="degree":
            centrality=list(dict(nx.degree(G)).values())
  
    # BETWENNESS 
    if node_color=="betweeness":
            centrality=list(dict(nx.betweenness_centrality(G)).values())
  
    # CLOSENESS
    if node_color=="closeness":
            centrality=list(dict(nx.closeness_centrality(G)).values())

    # NODE SIZE CAN COLOR
    node_colors = [cmap(u/(0.01+max(centrality))) for u in centrality]
    node_sizes = [4000*u/(0.01+max(centrality)) for u in centrality]

    # #PLOT NETWORK
    nx.draw(G,
            with_labels=True,
            edgecolors="black",
            node_color=node_colors,
            node_size=node_sizes,
            font_color='white',
            font_size=18,
            pos=pos
            )

    plt.show()

#------------------------------
# NETWORK SUMMARY FUNCTION
#------------------------------
def network_summary(G):

    def centrality_stats(x):
        x1=dict(x)
        x2=np.array(list(x1.values())); #print(x2)
        print("	min:" ,min(x2))
        print("	mean:" ,np.mean(x2))
        print("	median:" ,np.median(x2))
        # print("	mode:" ,stats.mode(x2)[0][0])
        print("	max:" ,max(x2))
        x=dict(x)
        sort_dict=dict(sorted(x1.items(), key=lambda item: item[1],reverse=True))
        print("	top nodes:",list(sort_dict)[0:6])
        print("	          ",list(sort_dict.values())[0:6])

    try: 
        print("GENERAL")
        print("	number of nodes:",len(list(G.nodes)))
        print("	number of edges:",len(list(G.edges)))

        print("	is_directed:", nx.is_directed(G))
        print("	is_weighted:" ,nx.is_weighted(G))


        if(nx.is_directed(G)):
            print("IN-DEGREE (NORMALIZED)")
            centrality_stats(nx.in_degree_centrality(G))
            print("OUT-DEGREE (NORMALIZED)")
            centrality_stats(nx.out_degree_centrality(G))
        else:
            print("	number_connected_components", nx.number_connected_components(G))
            print("	number of triangle: ",len(nx.triangles(G).keys()))
            print("	density:" ,nx.density(G))
            print("	average_clustering coefficient: ", nx.average_clustering(G))
            print("	degree_assortativity_coefficient: ", nx.degree_assortativity_coefficient(G))
            print("	is_tree:" ,nx.is_tree(G))

            if(nx.is_connected(G)):
                print("	diameter:" ,nx.diameter(G))
                print("	radius:" ,nx.radius(G))
                print("	average_shortest_path_length: ", nx.average_shortest_path_length(G))

            #CENTRALITY 
            print("DEGREE (NORMALIZED)")
            centrality_stats(nx.degree_centrality(G))

            print("CLOSENESS CENTRALITY")
            centrality_stats(nx.closeness_centrality(G))

            print("BETWEEN CENTRALITY")
            centrality_stats(nx.betweenness_centrality(G))
    except:
        print("unable to run")

#------------------------------
# ISOLATE GCC
#------------------------------
def isolate_GCC(G):
    comps = sorted(nx.connected_components (G),key=len, reverse=True) 
    nodes_in_giant_comp = comps[0]
    return nx.subgraph(G, nodes_in_giant_comp)

